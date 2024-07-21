      ****************************************************************
      *    PC-DATEN BEI ELEKTRONISCHEN VORSCHLAGSDATEN DEUTSCHE BANK
      ****************************************************************
      *    COPY-MEMBER: DRAN0289                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEU                       ! HEIDER   *
      * ANTRAG-939  !26.03.21! INTERNE TABELLE            ! HEIDER   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1246!02.11.21! PARKDEPOT (C+L)              ! RAHNER   *
      * ANTRAG-1401!03.02.22! WIEDERHOLUNG ÜBERGABE AN-1246! RAHNER  *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
           05  DB-PC-DATEN.
 
                10 DB-PC-HVANTNUM         PIC X(9).
                10 DB-PC-PIN              PIC S9(9)  COMP.
                10 DB-PC-PGEB-GES         PIC S9(4)  COMP.
                10 DB-PC-VB-GES           PIC S9(4)  COMP.
                10 DB-PC-VDABVERM         PIC S9(9)  COMP.
                10 DB-PC-VDABVERMPZ       PIC X(1).
                10 DB-PC-AFO-34C-ZEICHN   PIC S9(9)  COMP.
                10 DB-PC-VDVMNR           PIC S9(9)  COMP.
                10 DB-PC-VDVMNRPZ         PIC X(1).
                10 DB-PC-VDVMZUNA         PIC X(26).
                10 DB-PC-VDVMVORNA        PIC X(26).
                10 DB-PC-VDDIREKT         PIC S9(4)  COMP.
                10 DB-PC-PDVNZUNA         PIC X(26).
                10 DB-PC-PDVNVORNA        PIC X(26).
                10 DB-PC-PDVNPLZ          PIC X(6).
                10 DB-PC-PDVNORT          PIC X(30).
                10 DB-PC-PDVNLKZ          PIC X(3).
             
260321          10 DB-PC-PRODUKTE.
                
                   15 DB-PC-PROD1            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD1     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE1       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT1        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD1       PIC S9(6)V9(1)  COMP-3.
                   15 DB-PC-PROD2            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD2     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE2       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT2        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD2       PIC S9(6)V9(1)  COMP-3.
                   15 DB-PC-PROD3            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD3     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE3       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT3        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD3       PIC S9(6)V9(1)  COMP-3.
                   15 DB-PC-PROD4            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD4     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE4       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT4        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD4       PIC S9(6)V9(1)  COMP-3.
                   15 DB-PC-PROD5            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD5     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE5       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT5        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD5       PIC S9(6)V9(1)  COMP-3.
                   15 DB-PC-PROD6            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD6     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE6       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT6        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD6       PIC S9(6)V9(1)  COMP-3.
                   15 DB-PC-PROD7            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD7     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE7       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT7        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD7       PIC S9(6)V9(1)  COMP-3.
                   15 DB-PC-PROD8            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD8     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE8       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT8        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD8       PIC S9(6)V9(1)  COMP-3.
                   15 DB-PC-PROD9            PIC S9(4)  COMP.
                   15 DB-PC-BETRAG-PROD9     PIC S9(9)V9(2)  COMP-3.
                   15 DB-PC-ZAHLWEISE9       PIC S9(4)  COMP.
                   15 DB-PC-LAUFZEIT9        PIC S9(4)  COMP.
                   15 DB-PC-EINH-PROD9       PIC S9(6)V9(1)  COMP-3.
       
260321          10 DB-PC-PRODUKTE-TABELLE REDEFINES DB-PC-PRODUKTE.
                   15 DB-PC-PRODUKTE-TAB OCCURS 9.
                      20 DB-PC-PROD              PIC S9(4)      COMP.
                      20 DB-PC-BETRAG-PROD       PIC S9(9)V9(2) COMP-3.
                      20 DB-PC-ZAHLWEISE         PIC S9(4)      COMP.
                      20 DB-PC-LAUFZEIT          PIC S9(4)      COMP.
260321                20 DB-PC-EINH-PROD         PIC S9(6)V9(1) COMP-3. 
                
                10 DB-PC-VDABLEIN         PIC S9(6)V9(1)  COMP-3.
                10 DB-PC-DYNAMIK          PIC S9(3)V9(3)  COMP-3.
                10 DB-PC-HAND             PIC X(1).
                10 DB-PC-ANTRAG-DATUM     PIC X(10).
                10 DB-PC-KD-BEST-ID       PIC S9(18)V  COMP-3.
                10 DB-PC-ESIGN            PIC X(1).
                10 DB-PC-VERSION          PIC S9(4)  COMP.
                10 DB-PC-ERSTELLUNG       PIC X(26).
                10 DB-PC-KENNUNG-ERST     PIC X(8).
                10 DB-PC-LAST-UPDATE      PIC X(26).
                10 DB-PC-KENNUNG-AEND     PIC X(8).
                10 DB-PC-XGDV.
                   49 DB-PC-XGDV-LEN      PIC S9(4)  COMP.
                   49 DB-PC-XGDV-TEXT     PIC X(20000).
 
      *
      ****** ENDE COPY-MEMBER ****************************************
 


