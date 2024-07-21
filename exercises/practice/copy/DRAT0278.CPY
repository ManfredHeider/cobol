      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0278.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\DRAT0278.CPv  $
      *
      *   Rev 2.4   Jan 28 2000 11:28:18   T13EX44
      * 
      *
      *   Rev 2.3   Nov 24 1999 14:59:14   T13EX44
      *DTA-Sachbearbeiter
      *
      *   Rev 2.2   Apr 21 1999 17:34:34   T13EX22
      *ERGAENZUNGEN FUER PROTOKOLL
      *
      *   Rev 2.1   Mar 12 1999 09:30:06   T13EX22
      *Zähler für AKT-DGV eingefügt
      *
      *   Rev 2.0   Mar 12 1999 08:01:36   T13EX22
      *MFE-UMSTELLUNG
      *
      *   Rev 1.0   13 Jul 1998 09:28:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0278                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0023                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    07.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!07.09.98! 01 ! NEU                       ! WOLFF    *
      * APAA0012!24.09.98! 02 ! PMPE-VNR NEU              ! MUND     *
      * APA00012!08.02.98! 03 ! NEUE FELDER               ! MUND     *
      * APA00063!28.10.99!2.03! PDTA-ID ERGAENZT          ! SCHEIDEL *
      * APA00066!22.11.99!2.04! NEUE FELDER               ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0023 (PVW-BATCH).                           *
      *                                                              *
      ****************************************************************
      *
       01  DRAT0278-RESTART.
           05 DRAT0278-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRAT0278'.
      *
           05 DRAT0278-DATEN.
              10 DRAT0278-PMPG-ID                   PIC S9(015)
                                                        PACKED-DECIMAL.
240998        10 DRAT0278-PMPE-VNR                  PIC  X(015).
080299        10 DRAT0278-ANZ-PMPE-GES              PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PMPE-READ             PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PMPE-V                PIC S9(009) BINARY.
190499        10 DRAT0278-ANZ-PMPE-N                PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PMPE-N-V              PIC S9(009) BINARY.
              10 DRAT0278-ANZ-ERROR                 PIC S9(009) BINARY.
080299        10 DRAT0278-ANZ-PMPP-GES              PIC S9(009) BINARY.
080299        10 DRAT0278-ANZ-PMPP-V                PIC S9(009) BINARY.
190499        10 DRAT0278-ANZ-PMPP-N                PIC S9(009) BINARY.
080299        10 DRAT0278-ANZ-PMPP-N-V              PIC S9(009) BINARY.
080299        10 DRAT0278-ANZ-PMPP-ERROR            PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PVER-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PVOR-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PVER-UPDATE           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PVPT-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PEST-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PMBE-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PMPR-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PMPT-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PDPT-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PAMS-INSERT           PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PNAP-PM               PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PNAP-PR               PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PNAP-PT               PIC S9(009) BINARY.
110399        10 DRAT0278-ANZ-DGV-USED              PIC S9(009) BINARY.
110399        10 DRAT0278-ANZ-DGV-NOT-USED          PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PMPE-DEL              PIC S9(009) BINARY.
              10 DRAT0278-ANZ-PMPP-DEL              PIC S9(009) BINARY.
281099        10 DRAT0278-PDTA-ID                   PIC  9(015).
221199        10 DRAT0278-ANZ-PVPT-UPDATE           PIC S9(009) BINARY.
221199        10 DRAT0278-ANZ-PMDS-INSERT           PIC S9(009) BINARY.
