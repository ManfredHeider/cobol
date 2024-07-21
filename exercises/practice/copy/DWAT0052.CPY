      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0052.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0052.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:22   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   19 Mar 1998 10:54:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DWAT0052                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: RESTART                                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                        *
      *    DATUM:    11.03.1998                                       *
      *****************************************************************
      *    VERSION   1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!16.03.98!    ! NEU                       ! BRENNECKE*
      *         !04.09.98!    ! BR-Z-ANZ-... hinzugefügt  ! AUERBACH *
      *         !        !    !                           !          *
      *         !08.09.98!    ! AUFRUF A0000015           ! WOLFF    *
      *         !        !    !                           !          *
      *         !10.09.98!    ! SPEICHERUNG DES RC AUS    ! WOLFF    *
      *         !        !    ! BR81-RESTARTPGM-ANMELDEN  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
      *  DATENFELDER FUER RESTART                                  *
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
       01   K-UPAT0022              PIC X(008) VALUE 'UPAT0022'.

       01  FILLER.
           05  FILLER                 PIC  X(032)
                                    VALUE '*** RESTART            ***'.
           05  BR-Z-JOBNAME           PIC  X(008) VALUE SPACE.
           05  BR-Z-PROGRAMMNAME      PIC  X(008) VALUE SPACE.
           05  BR-Z-RC                PIC  X(003).
               88 BR-Z-RC-OK                VALUE 'OK '.
     *
     ***       RC NACH UNKONTROLLIERTEM PGM-ABBRUCH
               88 BRZ-RC-UNK-PGMABBRUCH      VALUE 'UPA'.
     *
     ***       RC NACH KONTROLLIERTEM PGM-ABBRUCH
               88 BR-Z-RC-K-PGMABBRUCH       VALUE 'KPA'.
100998*
100998     05  BR81-Z-RC                PIC  X(003).
100998         88 BR81-Z-RC-OK               VALUE 'OK '.
100998*
100998***      RC NACH UNKONTROLLIERTEM PGM-ABBRUCH
100998         88 BR81-Z-RC-UNK-PGMABBRUCH   VALUE 'UPA'.
100998*
100998***      RC NACH KONTROLLIERTEM PGM-ABBRUCH
100998         88 BR81-Z-RC-K-PGMABBRUCH     VALUE 'KPA'.
     *
     ***   ANZAHL DER VERARBEITETEN SäTZE SEIT DEM LETZTEN COMMIT
           05  BR-Z-ANZ-VERARB-SAETZE PIC S9(009) USAGE COMP
                                             VALUE ZERO.
     *
     ***   STANDARD-/DEFAULTWERT
           05  BR-Z-COMMITHAEUFIGKEIT PIC S9(009) USAGE COMP
                                             VALUE ZERO.
     *
     ***   ANZAHL DER DRUCHGEFüHRTEN COMMITS
           05  BR-Z-COMMITPOINT-NR    PIC S9(009) USAGE COMP
                                             VALUE ZERO.
           05  BR-Z-PROGRAMM-BEENDEN  PIC  X(001)
     *                                       VALUE SPACE.
           05  BR-Z-LAUFENDE-NR       PIC S9(004) USAGE COMP
                                             VALUE 1.
           05  BR-Z-RESTARTINFO       PIC  X(4000)
                                             VALUE SPACE.
      *
      ***  FUNKTIONEN FUER RESTART
       COPY DRAT0247.
      *
      ***  IN/OUT-PARAMETER DER RESTARTFUNKTIONEN
        COPY DRAT0248.
        COPY DRAT0249.
        COPY DRAT0250.
        COPY DRAT0251.
        COPY DRAT0252.
080998*
080998*    ----------------------------------------------------------
080998*    AUFRUF A0000015 - JOBNAME LESEN
080998*
080998 01  FILLER  PIC X(026)       VALUE '*** FUNKTION: DRAT0279 ***'.
080998     COPY DRAT0279.
      *
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DWAT0052) ********




