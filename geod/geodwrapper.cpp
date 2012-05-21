#include "geodwrapper.h"
#include "GeographicLib/Geodesic.hpp"

// static const GeographicLib::Geodesic &geod(GeographicLib::Geodesic::WGS84);

// "google sphere" / not faster (no special code path for sphere in geographiclib)
static const GeographicLib::Geodesic sphere(6378137,0);

static const GeographicLib::Geodesic* spheroid[]={
  &GeographicLib::Geodesic::WGS84,
  &sphere
};

double
geod_direct(unsigned s,
	    double lat1, double lon1, double azi1, double s12,
	    double* lat2, double* lon2, double* azi2,
	    double* m12, double* M12, double* M21, double* S12) {
  try {
    return spheroid[s]->Direct(lat1, lon1, azi1, s12,
			       *lat2, *lon2, *azi2,
			       *m12, *M12, *M21, *S12);
  }catch(...){
    // todo:
    return std::numeric_limits<double>::quiet_NaN();
  }
}

double
geod_inverse(unsigned s,
	     double lat1, double lon1, double lat2, double lon2,
	     double* s12, double* azi1, double* azi2, double* m12,
	     double* M12, double* M21, double* S12) {
  try{
    return spheroid[s]->Inverse(lat1, lon1, lat2, lon2,
			       *s12, *azi1, *azi2, *m12,
			       *M12, *M21, *S12);
  }catch(...){
    // todo:
    return std::numeric_limits<double>::quiet_NaN();
  }
}


