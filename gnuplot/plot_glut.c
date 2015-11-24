/* 
   This program emulates gnuplot_x11 by the use of GLUT,
   allows you to run gnuplot interacively in the Non-X11
   or Non-Tektronics graphc terminal environment but
   supports OpenGL/GLUT.

   By default, this program behaves slightly diffrent from
   the original gnuplot_x11. If invoked as 'gnuplot_x11'
   plot_glut enters the compatibility mode.
   'O' and 'Q' command switches compat and native mode, 
   respectively.

   MacOSX
    % cc -o plot_glut  -framework GLUT -flat_namespace plot_glut.c
    % cp plot_glut some/where/in/your/PATH/gnuplot_x11
    % cp plot_glut Gplt.app/Contens/MacOS/Gplt
    % open Gplt.app
    % echo -e "O\n" | telnet localhost 5055   # switch to gnuplot compat mode
    % gnuplot
    gnuplot> set term xlib
    gnuplot> set output "| telnet localhost 5055"

   Other Unixes
    % cc -o plot_glut plot_glut.c -lwhateveryouneed
    % cp plot_glut some/where/in/your/PATH/gnuplot_x11
    % gnuplot
    gnuplot> set term x11 [n]
       or
    gnuplot> set term xlib
    gnuplot> set output "| /full/path/to/gnuplot_x11"
       or
    % ./plot_glut &
    % echo -e "O\n" | telnet localhost 5055    # switch to gnuplot compat mode
    gnuplot> set term xlib
    gnuplot> set output "| telnet localhost 5055"


    Issues:
    - Some drawing cmds are not complete.
      (Fill, Text, line-width, type)
    - Optimal Redisplay, glFlush() timing.

    - GLUT API ver 3  requires at least one window
      to be created before glutMainLoop().
      In MacOSX10.2, it seems that all the windows need
      to be created before glutMainLoop().
      So, I changed window creation timing from 
      IdleFunc to init_graph() i.e., All MAX_GRAPHS
      windows are created hidden in init_graph(),
      and showed when nessesary in DisplayFunc.
      This works as expected in X11 (Linux and FreeBSD),
      but cause stray windows shown at the initialization 
      in MacOSX. 

                                    written by skimu@mac.com
 */

#ifdef __APPLE__
/* XXX - I don't like this kind of switch,
   but I could not find better solution. */
#include <GLUT/glut.h>   /* MacOSX uses non-standard path */
#else
#include <GL/glut.h>     /* This is what textbook says */
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

#define REDRAW_EACH_E_CMD 1

#define MAX_GRAPHS 8

enum {
  GR_NOT_USED = 0x00,
  GR_ACTIVE   = 0x01
};

struct graph {
  int s;
  int wid;
  int width;
  int height;
  struct plot_cmd *cmd_head;
  struct plot_cmd *cmd_tail;
} graph[MAX_GRAPHS];

enum {
  CMD_VECTOR = 0x100,
  CMD_MOVE   = 0x200,
  CMD_TEXT   = 0x300,
  CMD_FILL   = 0x400,
  CMD_JMODE  = 0x500,
  CMD_LINE_WIDTH = 0x600,
  CMD_LINE_TYPE  = 0x700,
  CMD_POINT0 = 0x800,
  CMD_POINT1 = 0x801,
  CMD_POINT2 = 0x802,
  CMD_POINT3 = 0x803,
  CMD_POINT4 = 0x804,
  CMD_POINT5 = 0x805,
  CMD_POINT6 = 0x806,
  CMD_POINT7 = 0x807,
  CMD_NOP    = 0xfff
};

enum JUSTIFY { /* taken from gnuplot-3.7/plot.h */
  J_LEFT,
  J_CENTRE,
  J_RIGHT
};

struct plot_cmd {
  struct plot_cmd *next;
  int cmd;
  union {
    struct {
      int j;
    } m1;
    struct {
      int x;  int y;
    } m2;
    struct {
      int x;  int y;
      char *str;
    } m3;
    struct {
      int x;  int y;
      int w;  int h;
      int sty;
    } m5;
  } u;
};

typedef struct plot_cmd plot_cmd;
typedef struct graph graph_type;

#define MAXCLIENT 8

struct client {
  int fd;
  int nbytes;
  char *buff;
  char *p;
  char *q;
} clients[MAXCLIENT];

struct sockaddr_in addr;
int cmd_port = 5055;
int lfd;

int verbose = 0;
int gplt_x11 = 0;

int default_window_width = 400;
int default_window_height = 400;

/* -------------------------------------------------------*/

int record(struct client *);
void init_graph();
int  get_graph_index(int w);
void activate_graph(int n);
void delete_cmds(graph_type *g);
void append_cmd(char *line_buff, graph_type *g);
void error(char *str);
int  get_xoffset();
int  get_yoffset();
int  create_graph(char *name);
void set_color(int type);
void draw_data_line(int x1, int y1, int x2, int y2, int width, int type);
void draw_solid_line(int x1, int y1, int x2, int y2, int width);
void draw_dotted_line(int x1, int y1, int x2, int y2, int width);
void draw_line(int x1, int y1, int x2, int y2, int width, int type);
void draw_text(int x, int y, char *str, int jmode, int px, int py);
void draw_fill(int x, int y, int w, int h, int type, int sty);
void draw_point0(int x, int y, int px, int py);
void draw_point1(int x, int y, int px, int py);
void draw_point2(int x, int y, int px, int py);
void draw_point3(int x, int y, int px, int py);
void draw_point4(int x, int y, int px, int py);
void draw_point5(int x, int y, int px, int py);
void draw_point6(int x, int y, int px, int py);
void parse_cmd(char *buffer, plot_cmd *c);
void print_plot_cmds(graph_type *g);
void print_cmds();
void exec_cmds(graph_type *g);
void display();
void reshape(int w, int h);
void key(unsigned char k, int x, int y);
void read_cmds();
void add_client(int);
void remove_client(int);

/* ------  graph & command structure operations ----- */

void init_graph()
{
  int i;
  char name[4];
  for (i=0; i < MAX_GRAPHS; i++) {
    graph[i].s = GR_NOT_USED;
    graph[i].wid = -1;
    graph[i].width = 1;
    graph[i].height = 1;
    graph[i].cmd_head = NULL;
    graph[i].cmd_tail = NULL;
  }
  for (i=0; i < MAX_GRAPHS; i++) {
    sprintf(name, "G%1d", i);
    graph[i].wid =  create_graph(name);
  }
}

int get_graph_index(int w)
{ 
  int i;
  for (i=0; i < MAX_GRAPHS; i++)
    if (graph[i].wid == w)
      break;
  return i;
}

void activate_graph(int n)
{
  glutSetWindow(graph[n].wid);
  if (graph[n].s != GR_ACTIVE) {
    graph[n].s = GR_ACTIVE;
    graph[n].width = default_window_width;
    graph[n].height = default_window_height;
    glutReshapeWindow(default_window_width, default_window_height);
    glutPositionWindow(get_xoffset(), get_yoffset());
    glutShowWindow();
  }
}

void delete_cmds(graph_type *g)
{
  plot_cmd *p, *psave;
  
  p = g->cmd_head;
  while (p != NULL) {
    psave = p->next;
    free(p);
    p = psave;
  }	  
  g->cmd_head = NULL;
  g->cmd_tail = NULL;
}

void append_cmd(char *line_buff, graph_type *g)
{
  plot_cmd cmd;

  parse_cmd(line_buff, &cmd);
  if (cmd.cmd == CMD_NOP) return;
  
  if (g->cmd_tail == NULL) {
    g->cmd_head = malloc(sizeof(plot_cmd));
    g->cmd_tail = g->cmd_head;
  } else {
    g->cmd_tail->next = malloc(sizeof(plot_cmd));
    g->cmd_tail = g->cmd_tail->next;
  }

  *g->cmd_tail = cmd; /* structure copy */
  g->cmd_tail->next = NULL;
}

/*  --------------------------------------------------- */

void error(char *str)
{
  fprintf(stderr, str);
  exit(-1);
}

/* Initial window position */

int get_xoffset()
{ 
  static int n = 0;
  return n++ * 410 + 20;
}

int get_yoffset()
{
  return 50;
}

int create_graph(char *name)
{
  int wid;

  wid = glutCreateWindow(name);
  if (verbose) fprintf(stderr, " %s created, wid is  %d\n", name, wid);
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(key);
  glutHideWindow();
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glShadeModel(GL_FLAT);
  return wid;
}

/* -------------------------------------------------------*/
void set_color(int type)
{
  switch (type) {
  case 2:
    glColor3f(1.0, 0.0, 0.0);
    break;
  case 3:
    glColor3f(0.0, 1.0, 0.0);
    break;
  case 4:
    glColor3f(1.0, 0.0, 1.0);
    break;
  case 5:
    glColor3f(1.0, 1.0, 0.0);
    break;
  case 6:
    glColor3f(0.0, 1.0, 1.0);
    break;
  case 7:
    glColor3f(1.0, 0.0, 1.0);
    break;
  case 8:
    glColor3f(1.0, 0.5, 0.5);
    break;
  case 9:
    glColor3f(0.5, 1.0, 0.5);
    break;
  default:
    glColor3f(1.0, 1.0, 1.0);
  }
}


void draw_data_line(int x1, int y1, int x2, int y2, int width, int type)
{
  set_color(type);
  glLineWidth((GLfloat)width);
  glBegin(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glEnd();
  set_color(0);
}

void draw_solid_line(int x1, int y1, int x2, int y2, int width)
{
  glColor3f(1.0, 1.0, 1.0);
  glLineWidth(1.0); /* ignore width */
  glBegin(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glEnd();
}

void draw_dotted_line(int x1, int y1, int x2, int y2, int width)
{
  glColor3f(1.0, 1.0, 1.0);
  glLineWidth((GLfloat)width);
  glEnable(GL_LINE_STIPPLE);
  glLineStipple(1, 0x0101);
    glBegin(GL_LINES);
      glVertex2i(x1, y1);
      glVertex2i(x2, y2);
    glEnd();
  glDisable(GL_LINE_STIPPLE);
}

void draw_line(int x1, int y1, int x2, int y2, int width, int type)
{
  /*  L   type  
     -2   0      Solid line (for frame)
     -1   1      dotted line (for frame)
      0   2      for data
      1   3      for data
      .   .      .
      .   .      .
      6   8      for data
      7   9      for data
      8   2      for data
      .   .      .
      .   .      .
  */
  if (type == 0) 
    draw_solid_line(x1, y1, x2, y2, width);
  else if (type == 1)
    draw_dotted_line(x1, y1, x2, y2, width);
  else 
    draw_data_line(x1, y1, x2, y2, width, type);
}

void draw_text(int x, int y, char *str, int jmode, int ww, int wh)
{
  /*
    GLUT Fontsize: 
    Top = 119.05, bottom = 33.33, width=104.76 
  */
  float offset = 0;
  float offsety = 0;
  float wscale = 0.35;                     /* adusted by experiment */
  float hscale = 0.7;                      /* adusted by experiment */

  switch (jmode) {
  case J_CENTRE:
    offset = strlen(str)*(-104.76/2.0)*wscale;
    break;
  case J_RIGHT:
    offset = strlen(str)*(-104.76)*wscale;
    break;
  case J_LEFT:
    offset = 0;
    break;
  default:;
  }
  offsety = -(119.05)/2.0*hscale; 
  offset += 20;                             /* adusted by experiment */

  glLoadIdentity();
  glTranslatef(x + offset, y + offsety, 0);
  glScalef(wscale, hscale, 1.0);
  while(*str != '\0') {
    glutStrokeCharacter(GLUT_STROKE_MONO_ROMAN, *str++);
  }
  glLoadIdentity();
}

void draw_fill(int x, int y, int w, int h, int type, int sty)
{
  set_color(type);
  glBegin(GL_POLYGON);
  glVertex2i(x,y);
  glVertex2i(x+w,y);
  glVertex2i(x+w,y+h);
  glVertex2i(x,y+h);
  glEnd();
  set_color(0);
}

void draw_point0(int x, int y, int px, int py)
{
  /* dot */
  glBegin(GL_POINTS);
  glVertex2i(x, y);
  glEnd();
}

void draw_point1(int x, int y, int px, int py)
{
  /* diamond */
  glBegin(GL_LINE_LOOP);
  glVertex2i(x - px, y);
  glVertex2i(x, y + py);
  glVertex2i(x + px, y);
  glVertex2i(x, y - py);
  glEnd();
}

void draw_point2(int x, int y, int px, int py)
{
  /* plus */
  glBegin(GL_LINES);
  glVertex2i(x - px, y);
  glVertex2i(x + px, y);
  glEnd();
  glBegin(GL_LINES);
  glVertex2i(x, y + py);
  glVertex2i(x, y - py);
  glEnd();
}

void draw_point3(int x, int y, int px, int py)
{
  /* box */
  glBegin(GL_LINE_LOOP);
  glVertex2i(x - px, y - py);
  glVertex2i(x - px, y + py);
  glVertex2i(x + px, y + py);
  glVertex2i(x + px, y - py);
  glEnd();
}

void draw_point4(int x, int y, int px, int py)
{
  /* X */
  glBegin(GL_LINES);
  glVertex2i(x - px, y - py);
  glVertex2i(x + px, y + py);
  glEnd();
  glBegin(GL_LINES);
  glVertex2i(x - px, y + py);
  glVertex2i(x + px, y - py);
  glEnd();
}

void draw_point5(int x, int y, int px, int py)
{
  /* triangle */
  int dy1, dy2;
  dy1 = (int) (((double)py)/1.7320508); /* 1.73.. = sqrt(3) */
  dy2 = (int) (((double)py)*2.0/1.7320508); 
  glBegin(GL_LINE_LOOP);
  glVertex2i(x + px, y - dy1);  
  glVertex2i(x - px, y - dy1); 
  glVertex2i(x, y + dy2);  
  glEnd();
}

void draw_point6(int x, int y, int px, int py)
{
  /* star */
  glBegin(GL_LINES);
  glVertex2i(x - px, y); 
  glVertex2i(x + px, y); 
  glEnd();
  glBegin(GL_LINES);
  glVertex2i(x, y - py); 
  glVertex2i(x, y + py); 
  glEnd();
  glBegin(GL_LINES);
  glVertex2i(x + px, y + py); 
  glVertex2i(x - px, y - py); 
  glEnd();
  glBegin(GL_LINES);
  glVertex2i(x + px, y - py); 
  glVertex2i(x - px, y + py); 
  glEnd();
}

/* -------------------------------------------------------*/

#define LBSIZE 512
char line_buff[LBSIZE];

#define READ_SIZE 512

/* get_line() : 
   retrun number of fresh bytes in the buffer,
   return -1 if EOF is read or error occured.
*/

int get_line(struct client *cli, char *dest, int len)
{
  int n;
  int c;

  if (cli->p == cli->q) {
    cli->p = cli->q = cli->buff;
    n = read(cli->fd, cli->q, READ_SIZE);
    if (n <= 0) {
      cli->nbytes = 0;
      return -1;
    }
    cli->q += n;
    cli->nbytes = n;
  }
  c = 0;
  while (cli->p < cli->q) {
    if (c > len) {
      if (verbose) {
        *--dest = 0;
        fprintf(stderr, "\n\n\"%s\"\n", line_buff);
      }
      error("Too long line");
    }
    if ((*cli->p == '\n') || 
        (*cli->p == '\r') || 
        (*cli->p == '\0')) {
      *dest++ = 0, c++;
      while (cli->p < cli->q) {
        if ((*cli->p != '\n') && 
            (*cli->p != '\r') && 
            (*cli->p != '\0'))
          break;
        cli->p++;
      }
      cli->nbytes = (cli->q - cli->p);
      return cli->nbytes;
    }
    *dest++ = *cli->p++;  c++;
  }
  if (cli->p != cli->q) error("something wrong in get_line\n");
  return get_line(cli, dest, len - c);
}

int more_line_p(struct client *cli)
{
  char *r, *s;
  for (r = cli->p, s = cli->q; r < s; r++)
    if ((*r == '\n') || (*r == '\r'))
      return 1;
  return 0;
}

/* ------------------------------------------------ */

int record(struct client *cli)
{
  static int cg = 0;
  int r;

  while ((r = get_line(cli, line_buff, LBSIZE)) >= 0) {
    if (verbose) {
      fprintf(stderr,"line_buff:%s\n", line_buff);
      fprintf(stderr,"line_buff:0x%02x\n", line_buff[0]);
    }

    switch (line_buff[0]) {
    case '\0':
      continue;

    case 'G': /* select graph */
      cg = line_buff[1] - '0';
      if (cg < 0 || cg >= MAX_GRAPHS) 
	cg = 0;
      activate_graph(cg);
      if (gplt_x11)
	delete_cmds(&graph[cg]);
      continue;

    case 'C': /* clear graph */
      cg = line_buff[1] - '0';
      if (cg < 0 || cg >= MAX_GRAPHS) 
	cg = 0;
      delete_cmds(&graph[cg]);
      return r;

    case 'O': /* glpt_x11 compat mode */
      gplt_x11 = 1;
      continue;

    case 'Q': /* plot_glut native mode */
      gplt_x11 = 0;
      continue;

    case 'E': /* end */
#if REDRAW_EACH_E_CMD
      glutPostRedisplay();
      return r;
#else
      continue;
#endif
    case 'R':
      glutPostRedisplay();
      return -2; /* End of commands */

    default:
      append_cmd(line_buff, &graph[cg]);
      continue;
    }
  }
#if REDRAW_EACH_E_CMD
#else
  glutPostRedisplay();
#endif

  return r;
}

void parse_cmd(char *buffer, plot_cmd *c)
{
  int x, y, style, w, h, l;

  switch (buffer[0]) {
  case 'V':
    if (sscanf(buffer, "V%4d%4d", &x, &y) != 2) return;
    c->cmd = CMD_VECTOR;
    c->u.m2.x = x;
    c->u.m2.y = y;
    break;

  case 'M':
    if (sscanf(buffer, "M%4d%4d", &x, &y) != 2) return;
    c->cmd = CMD_MOVE;
    c->u.m2.x = x;
    c->u.m2.y = y;
    break;

  case 'T':
    if (sscanf(buffer, "T%4d%4d", &x, &y) != 2) return;
    c->cmd = CMD_TEXT;
    c->u.m3.x = x;
    c->u.m3.y = y;
    
    l = strlen(buffer + 9);
    c->u.m3.str = malloc(l);
    strcpy(c->u.m3.str, buffer + 9);
    break;

  case 'F':
    if (sscanf(buffer, "F%4d%4d%4d%4d%4d", &style, &x, &y, &w, &h) != 5)
      return;
    c->cmd = CMD_FILL;
    c->u.m5.x = x;
    c->u.m5.y = y;
    c->u.m5.w = w;
    c->u.m5.h = h;
    c->u.m5.sty = style;
    break;

  case 'J':
    if (sscanf(buffer, "J%4d", &x) != 1) return;
    c->cmd = CMD_JMODE;
    c->u.m1.j = x;
    break;

  case 'W':
    if (sscanf(buffer, "W%4d", &w) != 1) return;
    c->cmd = CMD_LINE_WIDTH;
    c->u.m1.j =  w;
    break;

  case 'L':
    if (sscanf(buffer, "L%4d", &x) != 1) return;
    c->cmd = CMD_LINE_TYPE;
    c->u.m1.j = (x % 8) + 2; /* -2,-1 : special purpose */
    break;                   

  case 'P':
    if (sscanf(buffer, "P%1d%4d%4d", &w, &x, &y) != 3) return;
    w = w%8;
    c->cmd = CMD_POINT0 + w;
    c->u.m2.x = x;
    c->u.m2.y = y;
    break;

  default:
    c->cmd = CMD_NOP;
  }    
}

/* print_cmds, print_plot_cmds -- for debug purpose */

void print_plot_cmds(graph_type *g)
{
  plot_cmd *p;

  p = g->cmd_head;
  while (p != NULL) {
    switch (p->cmd) {
    case CMD_MOVE:
    case CMD_VECTOR:
    case CMD_POINT0:
    case CMD_POINT1:
    case CMD_POINT2:
    case CMD_POINT3:
    case CMD_POINT4:
    case CMD_POINT5:
    case CMD_POINT6:
    case CMD_POINT7:
      printf("cmd=%04x x=%4d y=%d\n", p->cmd, p->u.m2.x, p->u.m2.y);
      break;
    default:
      printf("cmd=%04x\n", p->cmd);
    }
    p = p->next;
  }
}

void print_cmds()
{
  int i;

  for (i=0; i < MAX_GRAPHS; i++) {
    if (graph[i].s == GR_NOT_USED) {
      printf("G%1d is not used.\n", i);
    } else {
      printf("Load commands for G%1d\n", i);
      print_plot_cmds(&graph[i]);
    }
  }
}

/* --------------------------------------------------------------- */

void exec_cmds(graph_type *g)
{
  plot_cmd *p;

  int x1=0, y1=0;
  int px, py;
  int l_width=0, l_type=0;
  int jmode;

  px = (int) (4096.0/(double)g->width);
  py = (int) (4096.0/(double)g->height);

  p = g->cmd_head;
  while (p != NULL) {
    switch (p->cmd) {
    case CMD_MOVE:
      x1 = p->u.m2.x;
      y1 = p->u.m2.y;
      break;

    case CMD_VECTOR:
      draw_line(x1, y1, p->u.m2.x, p->u.m2.y, l_width, l_type);
      x1 = p->u.m2.x;
      y1 = p->u.m2.y;
      break;

    case CMD_TEXT:
      draw_text(p->u.m3.x, p->u.m3.y, p->u.m3.str, jmode, px, py);
      break;

    case CMD_FILL:
      draw_fill(p->u.m5.x, p->u.m5.y, p->u.m5.w, p->u.m5.h, 
		l_type, p->u.m5.sty);
      break;
		
    case CMD_JMODE:
      jmode = p->u.m1.j;
      break;

    case CMD_LINE_WIDTH:
      l_width = p->u.m1.j;
      break;

    case CMD_LINE_TYPE:
      l_type = p->u.m1.j;
      break;

    case CMD_POINT0:
      draw_point0(p->u.m2.x, p->u.m2.y, px, py);
      break;

    case CMD_POINT1:
      draw_point1(p->u.m2.x, p->u.m2.y, px, py);
      break;

    case CMD_POINT2:
      draw_point2(p->u.m2.x, p->u.m2.y, px, py);
      break;

    case CMD_POINT3:
      draw_point3(p->u.m2.x, p->u.m2.y, px, py);
      break;

    case CMD_POINT4:
      draw_point4(p->u.m2.x, p->u.m2.y, px, py);
      break;

    case CMD_POINT5:
      draw_point5(p->u.m2.x, p->u.m2.y, px, py);
      break;

    case CMD_POINT6:
      draw_point6(p->u.m2.x, p->u.m2.y, px, py);
      break;

    case CMD_POINT7:
      /* denominator number 4 is adjusted by experiment */
      px = (int) (p->u.m2.x * 4096.0/(double)g->width/4);
      py = (int) (p->u.m2.y * 4096.0/(double)g->height/4);
      break;

    default:
      if (verbose) 
	fprintf(stderr, "cmd=%04x is not implemented yet\n", p->cmd);
    }
    p = p->next;
  }
  glFlush();
}

/*--------------------  Call backs ---------------------- */

void display()
{
  int i, w;

  glClear(GL_COLOR_BUFFER_BIT);
  glColor3f(1.0, 1.0, 1.0);

  w = glutGetWindow();
  i = get_graph_index(w);
  if (i >= MAX_GRAPHS) error("Error: could not find commands\n");
  if (graph[i].cmd_head != NULL)
    exec_cmds(&graph[i]);
  /*  glutSwapBuffers(); */
}

void reshape(int w, int h)
{
  int wid, i;

  wid = glutGetWindow();
  i = get_graph_index(wid);
  if(i >= MAX_GRAPHS) error("Something wrong\n");
  graph[i].width  = w;
  graph[i].height = h;

  glViewport(0, 0, (GLsizei) w, (GLsizei) h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0, 4096.0, 0,  4096.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

void key(unsigned char k, int x, int y)
{
  switch (k) {
  case 27:
  case 'q':
  case 'Q':
    exit(0);
    break;
  }
}

int accept_new_client(int s)
{
  int sc;
  int len;

  len = sizeof(addr);
  if (verbose) fprintf(stderr, "Accepting connection\n");
  sc = accept(s, (struct sockaddr *)&addr, &len);
  if (sc < 0)
    error("accept failed\n");
  if (verbose) fprintf(stderr, "Accepted connection fd=%d\n", sc);

  return sc;
}


void read_cmds()
{
  int fd;
  int n, i;
  fd_set fds;
  struct timeval timeout;

  FD_ZERO(&fds);
  FD_SET(lfd, &fds);
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  if (select(lfd+1, &fds, NULL, NULL, &timeout) < 0)
    error("select failed\n");

  if (FD_ISSET(lfd, &fds)) {
    fd = accept_new_client(lfd);
    add_client(fd);
  }

  FD_ZERO(&fds);
  n = 0;
  for (i=0; i < MAXCLIENT; i++) {
    if (clients[i].fd >= 0) {
      FD_SET(clients[i].fd, &fds);
      n = (clients[i].fd > n) ? clients[i].fd : n;
    }
  }
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  if (select(n+1, &fds, NULL, NULL, &timeout) < 0)
    error("select failed\n");

  for (i=0; i < MAXCLIENT; i++) {
    if (clients[i].fd >= 0) {
      if (FD_ISSET(clients[i].fd, &fds) ||
          more_line_p(&clients[i])) {
        if (record(&clients[i]) < 0) {
          remove_client(i);
        }
      }
    }
  }
}

int open_listening_socket()
{
    int sopt;
    int sl;

    if (verbose) fprintf(stderr, "Making listening port..\n");

    addr.sin_family = AF_INET;
    addr.sin_port = htons(cmd_port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    sl = socket(addr.sin_family, SOCK_STREAM, 0);
    if (sl < 0) error("socket failed\n");

    if (setsockopt(sl, SOL_SOCKET, SO_REUSEADDR, &sopt, sizeof(sopt)) < 0)
      error("setsockopt failted.\n");

    if (bind(sl, (struct sockaddr *)&addr, sizeof(addr)) < 0)
      error("bind failed\n");

    if (listen(sl, SOMAXCONN) < 0)
      error("listen failed\n");
    
    return sl;
}


void init_listener()
{
  lfd = open_listening_socket();
}

void add_client(int fd)
{
  int i;

  for (i=0; i < MAXCLIENT; i++) {
    if (clients[i].fd < 0) {
      clients[i].fd   = fd;
      clients[i].buff = malloc(READ_SIZE);
      clients[i].p    = clients[i].buff;
      clients[i].q    = clients[i].buff;
      clients[i].nbytes = 0;
      break;
    }
  }
  if (verbose) fprintf(stderr, "client added i=%d\n", i);

  if (i == MAXCLIENT)
    close(fd);
}

void remove_client(int i)
{
  close(clients[i].fd);
  clients[i].fd = -1;
  clients[i].nbytes = 0;
  free(clients[i].buff);
  clients[i].buff = NULL;
  clients[i].p = NULL;
  clients[i].q = NULL;
  if (verbose) fprintf(stderr, "client removed i=%d\n", i);
}

void init_clients(int init)
{
  int i;

  for (i=0; i < MAXCLIENT; i++) {
    clients[i].fd = -1;
    clients[i].nbytes = 0;
    clients[i].buff = NULL;
    clients[i].p = NULL;
    clients[i].q = NULL;
  }
  if (init >= 0)
    add_client(init);
}

/*--------------------   ---------------------- */

int main(int argc, char **argv)
{
  char *basename;

  if ((basename = rindex(argv[0], '/')) == NULL) {
    basename = argv[0];
  } else {
    basename++;
  }
  if (strcmp(basename, "gnuplot_x11") == 0) {
    /* gnuplot_x11 compativility mode */
    gplt_x11 = 1;
    default_window_width = 640;
    default_window_height = 480;
    init_clients(0);  /* stdin */
  } else if (strcmp(basename, "Gplt") == 0) {
    /* MacOSX application */
    init_clients(-1);
  } else {
    init_clients(0);
  }
  init_listener();

  glutInitWindowSize(1,1);
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE|GLUT_RGB);

  glutIdleFunc(read_cmds);
  init_graph();

  glutMainLoop();
  return 0;
}

/* EOF */
