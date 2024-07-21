      *****************************************************************
      *    test                                                       *
      *    COPY-MEMBER: DROK0143                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE FÜR PRODUKTGEBER GEIGER (79)         *
      *                                                               *
      *                                                               *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *                                                               *
      *    NACH BESPRECHUNG 20.11.2019 MIT AACHEN (RENE KUBE)
      *    + GEI-WS-START-DATUM  + GEI-WS-ENDE-DATUM                  *
      *    --------------------------------------------------------   *
      *****************************************************************
       01  EA-DATEN-GEI.

           03  GEI-DATEN.
               05  GEI-PGEB-GES             PIC 9(04).
               05  GEI-VB-GES               PIC 9(04).
               05  GEI-HVANTNUM             PIC X(09).
               05  GEI-PIN                  PIC 9(09).
               05  GEI-VDABVERM             PIC 9(07).
               05  GEI-VDABVERMPZ           PIC X(01).
               05  GEI-VDVMNR               PIC 9(07).
               05  GEI-VDVMNRPZ             PIC X(01).
               05  GEI-VDDIREKT             PIC 9(04).
               05  GEI-PDVNZUNA             PIC X(26).
               05  GEI-PDVNVORNA            PIC X(26).
               05  GEI-PDVNLKZ              PIC X(03).
               05  GEI-PDVNPLZ              PIC X(06).
               05  GEI-PDVNORT              PIC X(30).
               05  GEI-DRUCK-DATUM          PIC X(10).
               05  GEI-KUNDENNUMMER         PIC 9(18).
               05  GEI-PRODUKTE.
                   10 GEI-PRODUKT-TABELLE OCCURS 9.
                      15 GEI-PROD-PROD        PIC 9(04).
                      15 GEI-PROD-BETRAG-PROD PIC 9(09)V9(2).
                      15 GEI-PROD-ZAHLWEISE   PIC 9(04).
                      15 GEI-PROD-EINH-PROD   PIC 9(06)V9.
110619                15 GEI-ANZ-BARREN       PIC 9(04).
120919         05  GEI-VERSION              PIC 9(03).
120919         05  GEI-IBAN                 PIC X(34).
120919         05  GEI-NAME-BANK            PIC X(50).
120919         05  GEI-VNAME-KONTO-INH      PIC X(30).
120919         05  GEI-ZUNAME-KONTO-INH     PIC X(30).
120919         05  GEI-ANREDE               PIC X(05).
120919         05  GEI-TITEL                PIC X(30).
120919         05  GEI-GEB-DAT              PIC X(10).
120919         05  GEI-STRASSE              PIC X(45).
120919         05  GEI-EMAIL                PIC X(50).
120919         05  GEI-TELEFON              PIC X(30).
120919         05  GEI-FIRMA                PIC X(30).
120919         05  GEI-STEUERNUMMER         PIC X(13).
120919         05  GEI-ERSTELLZEITPUNKT     PIC X(26).
               05  GEI-WS-START-DATUM       PIC X(10).
               05  GEI-WS-ENDE-DATUM        PIC X(10).
120919         05  FILLER                   PIC X(01).
120919         05  GEI-BLACKBOX             PIC X(02).
      ****
      **** ENDE COPY DROK0143
      *****************************************************************



