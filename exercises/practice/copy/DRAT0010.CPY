      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0010.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0010.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:35:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00014000
      *    COPY-MEMBER: AUFRUFPARAMETER HILFE-DIALOG (DRAT0010)       * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00060000
      *    DATUM:    11.01.1993                                       * 00070000
      ***************************************************************** 00090000
      *    VERSION   1    09.02.95 16:40:20
      ****************************************************************  00092000
      * AENDERUNGEN:                                                 *  00093000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00094000
      * -------------------------------------------------------------*  00095000
      *         !19.03.93! 01 ! NEU                       ! SCHÖLLH. *  00096000
      * AFVB0163!08.02.95!    ! NEU: FT-IN-HSB            ! SCHÖLLH. *  00100101
      *         !        !    !                           !          *  00100200
      *         !        !    !                           !          *  00100300
      *         !        !    !                           !          *  00100400
      *         !        !    !                           !          *  00100500
      *         !        !    !                           !          *  00100600
      *                                                              *  00100700
      * ENDE-LOGBUCH                                                 *  00100800
      ****************************************************************  00100900
      *                                                                 00101000
      *    DIESE DATENSTRUKTUR WIRD ALS ARBEITSBEREICH FUER DEN         00110000
      *    AUFRUF DES HILFE-DIALOGS VERWENDET.                          00120000
      *    DIE WERTE WERDEN VOM DRIVER GEFUELLT, UEBER CADD-KOMM-VAR    00121000
      *    AN DEN HILFE-DIALOG UEBERGEBEN.                              00122000
      *                                                                 00123000
      *    ABHAENGIG VON PI-PGAT0003-OP SIND FOLGENDE PARAMETER         00124000
      *    ZU FÜLLEN:                                                   00125000
      *                            ! F1  ! HFG ! HSB !                  00126000
      *    ------------------------+-----+-----+-----+                  00127000
      *    PI-PGAT0003-OP          !  X  !  X  !  X  !                  00128000
      *    PI-PGAT0003-FELDNAME    !     !  X  !     !                  00129000
      *    PI-PGAT0003-MAPNAME     !     !  X  !     !                  00130000
      *    PI-PGAT0003-BILDNR      !  X  !     !     !                  00140000
      *    PI-PGAT0003-TRANS       !  X  !     !     !                  00150000
      *    PI-PGAT0003-FT-IN-HILFE !     !     !  X  !                  00151000
      *    ------------------------+-----+-----+-----+                  00160000
      *                                                                 00170000
      *    ERLÄUTERUNG ZU OP:                                           00171000
      *      F1   HILFEBILD IN EINEM STANDARD-DIALOG                    00172000
      *      HFG  HILFE FELDGRUPPE IN STANDARD-DIALOGEN                 00173000
      *      HSB  HILFE SONDERBILD; WIRD NUR IM HILFEDIALOG             00174000
      *            SELBST ANGEBOTEN                                     00175000
      *    ERLÄUTERUNG ZU FT-IN-HILFE:                                  00175100
      *      F1   BETÄTIGTE SONDERHILFE-FUNKTIONSTASTE IM HILFEDIALOG   00175200
      *                                                                 00176000
       01  PI-PGAT0003-HILFE-PARMS.                                     00180000
           05  PI-PGAT0003-OP            PIC  X(003) VALUE SPACE.       00190000
           05  PI-PGAT0003-FELDNAME      PIC  X(007) VALUE SPACE.       00200000
           05  PI-PGAT0003-MAPNAME       PIC  X(007) VALUE SPACE.       00210000
           05  PI-PGAT0003-BILDNR        PIC  9(002) VALUE ZERO.        00220000
           05  PI-PGAT0003-TRANS         PIC  X(004) VALUE SPACE.       00230000
           05  PI-PGAT0003-FT-IN-HILFE   PIC  X(003) VALUE SPACE.       00231000
      *                                                                 00240000
      ******** ENDE COPY-MEMBER AUFRUF HILFEDIALOG (DRAT0010) ********* 00250000
