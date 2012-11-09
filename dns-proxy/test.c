/*BINFMTC: -Wall -Wextra -Wno-unused-parameter
  test for getaddrinfo bug:
  
  getaddrinfo returns EAI_NONAME when it should return EAI_EAGAIN

  getaddrinfo returns EAI_NONAME when it should return EAI_EAGAIN for
  hosts with A but no AAAA record and there is packet loss and/or a an
  overloaded dns server

  to easily reproduce, fake packet loss/overloaded dns server
  on linux do something like:
  # iptables -I OUTPUT -p udp -m udp --dport 53 -j DROP 
  # iptables -I OUTPUT -p udp -m udp --dport 53 -j LOG --log-prefix "DROP DNS REQUEST " 
  # iptables -I OUTPUT -p udp -m udp --dport 53 -m limit --limit 10/sec -j ACCEPT 
  first
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>

/* test host must have a A record but no AAAA record
   also note the final dot - otherwise a search entry in
   resolv.conf might cause the first test to fail, too */
const char* host="karme.de.";

struct addrinfo hints; // all 0

int main(int argc, char** argv) {
  struct addrinfo* res;
  int i;
  hints.ai_family = AF_INET;
  /* first test should work on mosts setups
     (there is another bug involving mdns
     if it fails try to replace the hosts line in /etc/nsswitch.conf
     with something like:
     "hosts:          files dns"
     )
  */
#if 0
  for(i=0;i<10;++i) {
    int r=getaddrinfo(host,0,&hints,&res);
    if(!((r==0)||(r==EAI_AGAIN))) {
      printf("%s:%d: error: r=%d %s\n",__FILE__,__LINE__,r,gai_strerror(r));
      exit(EXIT_FAILURE);
    }
    if (!r) freeaddrinfo(res);
  }
#endif
  /* second test will fail sometimes
     what happens?
     DNS request for A record is sent but no answer is received
     DNS request for AAAA record is sent and answer without entries received
     now getaddrinfo returns EAI_NONAME when in fact it should return EAI_AGAIN
  */
  hints.ai_family = AF_UNSPEC;
  for(i=0;i<10;++i) {
    int r=getaddrinfo(host,0,&hints,&res);
    if(!((r==0)||(r==EAI_AGAIN))) {
      printf("%s:%d: error: r=%d %s\n",__FILE__,__LINE__,r,gai_strerror(r));
      exit(EXIT_FAILURE);
    }
    if (!r) freeaddrinfo(res);
  }
  return EXIT_SUCCESS;
}
