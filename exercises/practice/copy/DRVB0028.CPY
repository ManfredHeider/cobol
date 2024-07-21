      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0028.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0028.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:08:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001004
      *    COPY-MEMBER: DRVB0028                                      * 00002004
      *    --------------------------------------------------------   * 00003004
      *    SCHNITTSTELLE UPVB0016 BEARBEITEN BESCHRÄNKUNGEN           * 00004004
      *    --------------------------------------------------------   * 00005004
      *    ERSTELLT: GIESE                                            * 00006004
      *    DATUM:    24.04.1993                                       * 00007004
      ***************************************************************** 00009004
      *    VERSION   01   15.10.93                                   *  00009115
      ****************************************************************  00009213
      * AENDERUNGEN:                                                 *  00009313
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009413
      * -------------------------------------------------------------*  00009513
      *         !16.09.93! 01 ! NEU                       ! giese    *  00009613
      *         !12.10.93!    ! neue 88er stufe           ! giese    *  00009714
      *         !15.10.93! 02 ! PROD.ÜBERGABE             ! HECKER   *  00009815
      *         !        !    !                           !          *  00009913
      *         !        !    !                           !          *  00010013
      *         !        !    !                           !          *  00010113
      *         !        !    !                           !          *  00010213
      *         !        !    !                           !          *  00010313
      *         !        !    !                           !          *  00010413
      *         !        !    !                           !          *  00010513
      *                                                              *  00010613
      * ENDE-LOGBUCH                                                 *  00010713
      ****************************************************************  00010813
      *                                                                 00010904
      *   IN:                                                           00011006
      *   PI-UPVB0016-MA-ST-NR        Mitarbeiternummer                 00011106
      *                                                                 00011206
      *   PI-UPVB0016-BETR-ST-NR-ALT  Die Betreuernummer des            00011306
      *                               Mitarbeiters vor dem Überholen    00011406
      *                               bzw. beantragten Betreuerwechsel  00011506
      *                                                                 00011606
      *   PI-UPVB0016-BETR-ST-NR-NEU  Die Nummer des neuen Betreuers    00011706
      *                                                                 00011806
      *   PI-UPVB0016-FKT                                               00011912
121093*     88 UPVB16-FKT-SET-BEANTR-BONUS                              00012014
      *                               MIT DEM ERREICHEN EINER BONUS-    00012114
      *                               STUFE WIRD BES=90 GESETZT         00012214
      *                                                                 00012306
      *     88 CHECK-BEANTR-BONUS     Bei Höherstufung von VBA zu AL    00012414
      *                               werden alle BES = 90 gelöscht     00012514
      *                                                                 00012614
      *     88 CHECK-UEBERHOLEN       Betreuerwechsel durch Überholen   00012706
      *                               Der Eintrag erfolgt beim          00012806
      *                               Überholten                        00012906
      *                                                                 00013006
      *     88 CHECK-BETREUERWECHSEL  Beantragter Betreuerwechsel       00013106
      *                               Mit und ohne Direktionswechsel    00013206
      *                               Einträge erfolgen sowohl beim     00013306
      *                               Antragsteller und bei dem neuen   00013406
      *                               Betreuer und dessen Betreuer      00013506
      *                                                                 00013612
      *   PI-UPVB0016-CICS-timestamp                                    00013712
      *   PI-UPVB0016-USER-ID          ==> Diese Daten werden für das   00013812
      *   PI-UPVB0016-TERMINAL-ID          Schreibmodul gebraucht       00013912
      *                                                                 00014006
      *    ABHAENGIG VON PI-UPVB0016-FKT SIND ZU FUELLEN:               00014112
      *                                                                 00014212
      *    FKT                         ! BB! UH! BW! BS!                00014314
      *    ----------------------------+---+---+---+---+                00014414
      *    PI-UPVB0016-FKT             ! X ! X ! X ! X !                00014514
      *    PI-UPVB0016-MA-ST-NR        ! X ! X ! X ! X !                00014614
      *    PI-UPVB0016-MA-DIR-NR       !   !   ! X !   !                00014714
      *    PI-UPVB0016-BETR-ST-NR-ALT  !   ! X ! X !   !                00014814
      *    PI-UPVB0016-BETR-ST-NR-NEU  !   !   ! X !   !                00014914
      *    PI-UPVB0016-USER-ID         ! X ! X ! X ! X !                00015014
      *    PI-UPVB0016-TERMINAL-ID     ! X ! X ! X ! X !                00015114
      *    PI-UPVB0016-CICS-TIMESTAMP  ! X ! X ! X ! X !                00015214
      *    ----------------------------+---+---+---+---+                00015314
      *                                                                 00016012
      *                                                                 00018006
      *   out:                                                          00018106
      *   PO-UPVB0016-RETCODE        0 --> OK                           00018208
      *                              1 --> PARAMTER FEHLER              00018308
      *                              2 --> FEHLER BEI SCHREIBMODUL      00018408
      *   PO-UPVB0016-PR-PARAMETER   RÜCKGABE-BEREICH VON SCHREIBMODUL  00018508
      *                                                                 00018608
      *                                                                 00018712
       01  PI-UPVB0016.                                                 00018804
           05 PI-UPVB0016-MA-ST-NR       PIC S9(09) COMP.               00018904
           05 PI-UPVB0016-MA-DIR-NR      PIC S9(04) COMP.               00019009
           05 PI-UPVB0016-BETR-ST-NR-ALT PIC S9(09) COMP.               00019106
           05 PI-UPVB0016-BETR-ST-NR-NEU PIC S9(09) COMP.               00019206
           05 PI-UPVB0016-FKT            PIC  X(02).                    00019312
121093        88 UPVB16-FKT-SET-BEANTR-BONUS  VALUE 'SB'.               00019414
              88 CHECK-BEANTR-BONUS           VALUE 'BB'.               00019514
              88 CHECK-UEBERHOLEN             VALUE 'UH'.               00019614
              88 CHECK-BETREUERWECHSEL        VALUE 'BW'.               00019714
                                                                        00019804
           05 PI-UPVB0016-CICS-TIMESTAMP  PIC  X(26).                   00019910
           05 PI-UPVB0016-USER-ID        PIC  X(08).                    00020004
           05 PI-UPVB0016-TERMINAL-ID    PIC  X(08).                    00020104
      *                                                                 00020204
       01  PO-UPVB0016.                                                 00020304
           05  PO-UPVB0016-RETCODE           PIC  9(03).                00020408
           05  PO-UPVB0016-PR-PARAMETER      PIC  X(82).                00020508
      *                                                                 00021004
      ******** ENDE COPY-MEMBER (DRVB0028) *******************          00030004
