      *****************************************************************
      *    COPY-MEMBER: DRAN0395                                     
      *                                                              
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE FÜR PRODUKTGEBER DB (60)             *
      *                                       UND DWS (48)            *
      *                                                               *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCAN0026)   *
      *    FÜR DATENSÄTZE AB VERSION 2101                            
      *    --------------------------------------------------------   *
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1222!28.10.21! NEU                          ! RAHNER   *
      * ANTRAG-1401!03.02.22! WIEDERHOLUNG ÜBERGABE AN-1246! RAHNER  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  EA-DATEN-DB.
 
           03  DB-DATEN.
               05  DB-VERSION              PIC 9(04).
               05  DB-HVANTNUM             PIC X(09).
               05  DB-PIN                  PIC 9(09).
               05  DB-VB-GES               PIC 9(04).
               05  DB-VDABVERM             PIC 9(09).
               05  DB-VDABVERMPZ           PIC X(01).
               05  DB-AFO-34C-ZEICHN       PIC 9(09).
               05  DB-VDVMNR               PIC 9(09).
               05  DB-VDVMNRPZ             PIC X(01).
               05  DB-VDVMZUNA             PIC X(26).
               05  DB-VDVMVORNA            PIC X(26).
               05  DB-VDDIREKT             PIC 9(04).
               05  DB-PDVNZUNA             PIC X(26).
               05  DB-PDVNVORNA            PIC X(26).
               05  DB-PDVNPLZ              PIC X(06).
               05  DB-PDVNORT              PIC X(30).
               05  DB-PDVNLKZ              PIC X(03).
               05  DB-PIN-NP               PIC 9(09).
               05  DB-EINH-GESAMT          PIC 9(06)V9.
               05  DB-DYNAMIK              PIC 9(03)V9(3).
               05  DB-ANTRAG-DATUM         PIC X(10).
               05  DB-KUNDENBESTANDSID     PIC 9(18).
      *
               05  DB-HAUPT-PRODUKTE.
                   10 DB-HP-TABELLE OCCURS 25.
                      15 DB-HP-PGEB-GES      PIC 9(04).
                      15 DB-HP-PROD          PIC 9(04).
                      15 DB-HP-BETRAG        PIC 9(09)V9(2).
                      15 DB-HP-ZAHLWEISE     PIC 9(04).
                      15 DB-HP-LAUFZEIT      PIC 9(04).
                      15 DB-HP-EINH-PROD     PIC 9(06)V9.
                      15 DB-HP-RABATT        PIC 9(03).
                      15 DB-HP-KZ-PARKDEPOT  PIC X(01).
      *
               05  DB-NEBEN-PRODUKTE.
                   10 DB-NP-TABELLE OCCURS 25.
                      15 DB-NP-PGEB-GES      PIC 9(04).
                      15 DB-NP-PROD          PIC 9(04).
                      15 DB-NP-BETRAG-PROD   PIC 9(09)V9(2).
                      15 DB-NP-ZAHLWEISE     PIC 9(04).
                      15 DB-NP-LAUFZEIT      PIC 9(04).
                      15 DB-NP-EINH-PROD     PIC 9(06)V9.
                      15 DB-NP-RABATT        PIC 9(03).
                      15 DB-NP-KZ-PARKDEPOT  PIC X(01).
      *
               05 FILLER                   PIC X(91).
               05 DB-BLACKBOX              PIC X(02).
      *****************************************************************
      *---------------------- ENDE DRAN0395 --------------------------* 
      *****************************************************************
