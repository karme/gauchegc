#ifndef GEODWRAPPER_H
#define GEODWRAPPER_H

#ifdef __cplusplus
extern "C" {
#endif

double
geod_direct(unsigned s,
	    double lat1, double lon1, double azi1, double s12,
	    double* lat2, double* lon2, double* azi2,
	    double* m12, double* M12, double* M21, double* S12);

double
geod_inverse(unsigned s,
	     double lat1, double lon1, double lat2, double lon2,
	     double* s12, double* azi1, double* azi2, double* m12,
	     double* M12, double* M21, double* S12);

#ifdef __cplusplus
}
#endif

#endif
