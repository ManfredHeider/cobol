      *    TEILMASCHINELLE DATEN MIT
      ****************************************************************
      *    COPY-MEMBER: DRAN0097                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1 ! NEU                       ! RAHNER   *
      * AAN00684!31.08.06!2.2 ! KUNDENBESTANDS-ID FÜR AVS ! RAHNER   *
      * AAN00730!30.11.07!2.3 ! ALLFINANZ                 ! RAHNER   *
      * AAN01211!09.03.15!2.5 ! ESIGN, ZAHLWEISE, LAUFZEIT! HEIDER   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-939 !26.03.21! INTERNE TABELLE              ! HEIDER   *
      * ANTRAG-1246!02.11.21! PARKDEPOT (VERSION 2101) NEU ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
                05  DWS-PC-DATEN.
 
      *        GLEICHHEITSPRÜFUNG
                10 DWS-PC-PGEB-GES          PIC S9(4) COMP.
                10 DWS-PC-PIN               PIC S9(9) COMP.
                10 DWS-PC-HVANTNUM          PIC X(9).
                10 DWS-PC-VDABVERM          PIC S9(9) COMP.
                10 DWS-PC-VDABVERMPZ        PIC X.
                10 DWS-PC-VDVMNR            PIC S9(9) COMP.
                10 DWS-PC-VDVMNRPZ          PIC X.
                10 DWS-PC-VDVMZUNA          PIC X(26).
                10 DWS-PC-VDVMVORNA         PIC X(26).
                10 DWS-PC-VDDIREKT          PIC S9(4) COMP.
                10 DWS-PC-PDVNZUNA          PIC X(26).
                10 DWS-PC-PDVNVORNA         PIC X(26).
                10 DWS-PC-PDVNPLZ           PIC X(6).
                10 DWS-PC-PDVNORT           PIC X(30).
                10 DWS-PC-PDVNLKZ           PIC X(3).
             
260321          10 DWS-PC-PRODUKTE.
                   15 DWS-PC-PROD1             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD1         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD1        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE1        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT1         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT1           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT1     PIC X(1).
                   15 DWS-PC-PROD2             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD2         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD2        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE2        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT2         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT2           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT2     PIC X(1).
                   15 DWS-PC-PROD3             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD3         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD3        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE3        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT3         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT3           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT3     PIC X(1).
                   15 DWS-PC-PROD4             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD4         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD4        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE4        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT4         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT4           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT4     PIC X(1).
                   15 DWS-PC-PROD5             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD5         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD5        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE5        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT5         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT5           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT5     PIC X(1).
                   15 DWS-PC-PROD6             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD6         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD6        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE6        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT6         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT6           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT6     PIC X(1).
                   15 DWS-PC-PROD7             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD7         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD7        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE7        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT7         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT7           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT7     PIC X(1).
                   15 DWS-PC-PROD8             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD8         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD8        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE8        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT8         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT8           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT8     PIC X(1).
                   15 DWS-PC-PROD9             PIC S9(4) COMP.
                   15 DWS-PC-SUM-PROD9         PIC S9(9) COMP-3.
                   15 DWS-PC-EINH-PROD9        PIC S9(6)V9(1) COMP-3.
                   15 DWS-PC-ZAHLWEISE9        PIC S9(4) COMP.
                   15 DWS-PC-LAUFZEIT9         PIC S9(4) COMP.
021121             15 DWS-PC-RABATT9           PIC S9(4) COMP.
021121             15 DWS-PC-KZ-PARKDEPOT9     PIC X(1).
                10 DWS-PC-PRODUKTE-TABELLE REDEFINES DWS-PC-PRODUKTE.
                   15 DWS-PC-PRODUKTE-TAB OCCURS 9.
                      20 DWS-PC-PROD              PIC S9(4) COMP.
                      20 DWS-PC-SUM-PROD          PIC S9(9) COMP-3.
                      20 DWS-PC-EINH-PROD         PIC S9(6)V9(1) COMP-3.
                      20 DWS-PC-ZAHLWEISE         PIC S9(4) COMP.
260321                20 DWS-PC-LAUFZEIT          PIC S9(4) COMP.
021121                20 DWS-PC-RABATT            PIC S9(4) COMP.
021121                20 DWS-PC-KZ-PARKDEPOT      PIC X(1).
                   
                10 DWS-PC-VDABLEIN          PIC S9(6)V9(1) COMP-3.
                10 DWS-PC-AFO-34C-VBNR      PIC S9(9) COMP.
                10 DWS-PC-HAND              PIC X.
                10 DWS-PC-ANTRAGS-DATUM     PIC X(10).
                10 DWS-PC-ERSTELLUNG        PIC X(26).
                10 DWS-PC-KENNUNG-ERST      PIC X(8).
                10 DWS-PC-KENNUNG-AEND      PIC X(8).
                10 DWS-PC-LAST-UPDATE       PIC X(26).
                10 DWS-PC-KD-BEST-ID        PIC S9(18)V COMP-3.
                10 DWS-PC-VB-GES            PIC S9(4) COMP.
                10 DWS-PC-ESIGN             PIC X.
                10 DWS-PC-XGDV.
                   15 DWS-PC-XGDV-LEN       PIC S9(4) COMP.
                   15 DWS-PC-XGDV-TEXT      PIC X(20000).
      *
      ****** ENDE COPY-MEMBER ****************************************
 

