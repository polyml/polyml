/NOT SUPPORTED/ { exit }

{ if ( NF >= 2 && $3 != "(callback)" )
  {
    if ( $3 == "Atom" )           $3 = "int" ;
    if ( $3 == "Cardinal" )       $3 = "int" ;
    if ( $3 == "Dimension" )      $3 = "int" ;
    if ( $3 == "KeySym" )         $3 = "int" ;
    if ( $3 == "Pixel" )          $3 = "int" ;
    if ( $3 == "Position" )       $3 = "int" ;
    if ( $3 == "XmTextPosition" ) $3 = "int" ;
    if ( $3 == "short" )          $3 = "int" ;

    if ( $4 == "list" )
      printf ("exception %s of %s list ;\n",$1,$3) ;
    else
      printf ("exception %s of %s ;\n",$1,$3) ;
  }
}
