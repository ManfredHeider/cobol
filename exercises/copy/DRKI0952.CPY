      ****************************************************************
      *    COPY-MEMBER: DRKI0952 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    14.02.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068!14.02.00! 01 !NEU                        ! WHO      *
      * AKI00474!23.03.05!2.2 !KRTRISIKOZUSCHLAG auf X(10)! IFR      *
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9561   !11.01.22! KRTKVTARIFARTID IN           ! T13AC20  
      *            !        ! KRTTARIFART-KNR UMBENANNT,   !          
      *            !        ! KRTTARIFART-KTY AUFGENOMMEN  !          
      * -----------!--------!------------------------------!----------
      * DBO-10102  !20.06.22! LÖSCHEN DER FELDER           ! U131848
      *            !        ! KRTGUTSCHRIFT                !
      *            !        ! KRTGUTSCHRWKZ                !
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0952                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0952.
           05  PI-DRKI0952-PROGRAMM             PIC  X(008).
           05  PI-DRKI0952-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI0952-DATEN.
               10 PI-DRKI0952-KRTVERTRAGSID     PIC  X(013).
      ***
       01 PO-DRKI0952.
          05 PO-DRKI0952-RC                     PIC  X(003).
             88 PO-DRKI0952-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0952-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0952-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0952-DATEN.
             10 PO-DRKI0952-KRTVERSTARLFDNR     PIC S9(004)      COMP.
             10 PO-DRKI0952-KRTTARIFART-KNR     PIC S9(004)      COMP.
             10 PO-DRKI0952-KRTDATABGANGVERTR   PIC  X(010).
             10 PO-DRKI0952-KRTDATZUGANGVERTR   PIC  X(010).
             10 PO-DRKI0952-KRTMONATSBEITRAG    PIC S9(009)V9(2) COMP-3.
             10 PO-DRKI0952-KRTMONBEITRWKZ      PIC  X(001).
             10 PO-DRKI0952-KRTSELBSTBETEILIG   PIC S9(009)V9(2) COMP-3.
             10 PO-DRKI0952-KRTSELBSTBETWKZ     PIC  X(001).
             10 PO-DRKI0952-KRTTARIFNAMEN       PIC  X(008).
230305*      10 PO-DRKI0952-KRTRISIKOZUSCHLAG   PIC  X(001).
230305       10 PO-DRKI0952-KRTRISIKOZUSCHLAG   PIC  X(010).
             10 PO-DRKI0952-KRTVERSLEISTUNG     PIC S9(009)V9(2) COMP-3.
             10 PO-DRKI0952-KRTVERSLSTWKZ       PIC  X(001).
             10 PO-DRKI0952-KRTTARIFART-KTY     PIC S9(004)      COMP.
      ******** Ende Copy-Member **************************************
