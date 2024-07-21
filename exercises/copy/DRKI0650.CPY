      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0650                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZXKI0313 (GUTHABENANTEIL)   *
      *                                                               *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                          *
      *    DATUM:    28.10.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00043!15.09.99!      NEUERSTELLUNG             ! PHILIPP  *
      *DBO-10080!09.08.22!Feldlängenänderung              !u131774   *
      *         !        !GHAGUTHABENANT S9(7)V9(4) COMP-3!          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0650                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0650.
          05 PI-DRKI0650-PROGRAMM               PIC X(008).
          05 PI-DRKI0650-UFKT-CODE              PIC 9(002).
          05 PI-DRKI0650-DATEN.
             10 PI-DRKI0650-GHAVERTRAGSID       PIC X(013).
      *
       01 PO-DRKI0650.

          05 PO-DRKI0650-RC                     PIC  X(003).
             88 PO-DRKI0650-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0650-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI0650-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI0650-DATEN.
            10 PO-DRKI0650-GHAFONDID            PIC S9(4) COMP.
            10 PO-DRKI0650-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.
            10 PO-DRKI0650-GHALFDNR             PIC S9(4) COMP.
            10 PO-DRKI0650-GHAGUTHABEN          PIC S9(9)V9(2) COMP-3.
            10 PO-DRKI0650-GHAGUTHABENANT       PIC S9(7)V9(4) COMP-3.
            10 PO-DRKI0650-GHAGUTHABENWKZ       PIC X(01).
      *
      ***** Ende Copy-Member DRKI0650 *****************************
      *---------------------------------------------------------------*
