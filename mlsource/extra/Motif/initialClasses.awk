/NOT SUPPORTED/ { exit }

{ if ( NF >= 2 && $3 != "(callback)" )
  {
    printf ("val %s = \"%s\" ;\n",$2,substr($2,4,length($2)-3)) ;
  }
}
