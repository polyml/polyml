/NOT SUPPORTED/ { exit }

{ if ( NF >= 2 && $3 != "(callback)" )
  {
    printf ("val %s: string ;\n",$2) ;
  }
}
