      *AUTHOR.        T13AC20.
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * DBO8328 !09.04.21!NEU                              !T13AC20
      *         !        !                                 !
      *         !        !                                 !
      *         !        !                                 !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *****************************************************************
      *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0553
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0553
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: SELECT HAUPTVERTRAGSTEIL ZU VERTRAGSID
      *
      *****************************************************************
      *                                                                 
       01  PI-DRKI0553.                                                 
           05 PI-DRKI0553-KEY.                                          
              10 PI-DRKI0553-LPTVERTRAGSID       PIC  X(013).          
      *                                                                 
       01  PO-DRKI0553.                                                 
           05 PO-DRKI0553-RC                     PIC  X(003).          
              88 PO-DRKI0553-RC-OK                      VALUE 'OK '.   
              88 PO-DRKI0553-RC-NOT-FOUND               VALUE 'NFD'.   
      *                                                                 
           05 PO-DRKI0553-NI.
              15 PO-DRKI0553-NI-LPTBEGINN        PIC S9(004) BINARY.
              15 PO-DRKI0553-NI-LPTABLAUF        PIC S9(004) BINARY.
              15 PO-DRKI0553-NI-LPTABLBZ         PIC S9(004) BINARY.
              15 PO-DRKI0553-NI-LPTTARIFBEITRAG  PIC S9(004) BINARY.
              15 PO-DRKI0553-NI-LPTRTBTGVT       PIC S9(004) BINARY.
              15 PO-DRKI0553-NI-LPTZAHLBTGVT     PIC S9(004) BINARY.
              15 PO-DRKI0553-NI-LPTVZUST-D-DAT   PIC S9(004) BINARY.
      *
           05  PO-DRKI0553-DATEN.
             10 PO-DRKI0553-LPTLFDNR             PIC S9(004) BINARY.
             10 PO-DRKI0553-LETZTEAENDERUNG      PIC  X(026).
             10 PO-DRKI0553-LPTBEZUGLFDNR        PIC S9(004) BINARY.
             10 PO-DRKI0553-LPTHVT-KZN           PIC  X(001).
             10 PO-DRKI0553-LPTTARKLASSE-KTY     PIC S9(004) BINARY.
             10 PO-DRKI0553-LPTTARKLASSE-KNR     PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRKI0553-LPTBEGINN            PIC  X(010).
             10 PO-DRKI0553-LPTABLAUF            PIC  X(010).
             10 PO-DRKI0553-LPTBEITRZUST-KTY     PIC S9(004) BINARY.
             10 PO-DRKI0553-LPTBEITRZUST-KNR     PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRKI0553-LPTABLBZ             PIC  X(010). 
             10 PO-DRKI0553-LPTTARIFBEITRAG      PIC S9(011)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRKI0553-LPTRTBTGVT           PIC S9(011)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRKI0553-LPTZAHLBTGVT         PIC S9(011)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRKI0553-LPTVERSSUMME         PIC S9(011)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRKI0553-LPTVZUST-D-KTY       PIC S9(004) BINARY.
             10 PO-DRKI0553-LPTVZUST-D-KNR       PIC S9(004) BINARY.
             10 PO-DRKI0553-LPTVZUST-D-DAT       PIC  X(010).