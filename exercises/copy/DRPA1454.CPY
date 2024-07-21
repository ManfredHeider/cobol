      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1454.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1454.CPv  $
      *
      *   Rev 2.2   Jun 27 2003 09:27:12   t13ae46
      *FPA00601: P0-DRPA1454-PMPG-VERSION hinzu
      *
      *   Rev 2.1   05 Feb 2001 10:21:44   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.0   04 Jan 2001 11:48:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1454                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<NPM ERMITTELN       >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    08.01.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0118!08.01.01! 01 ! NEU                       ! MUND     *
      * FPA00601!26.06.03! 02 ! P0-DRPA1454-PMPG-VERSION  ! WANG     *
      *         !        !    ! HINZU                     !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1454                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1454.
          05 PI-DRPA1454-DATEN.
              10 PI-DRPA1454-PGEB-GES       PIC S9(004) BINARY.
              10 PI-DRPA1454-PMPG-ID        PIC S9(015) PACKED-DECIMAL.
              10 PI-DRPA1454-PMPG-VERSION   PIC S9(004) BINARY.

       01 PO-DRPA1454.
          05 PO-DRPA1454-RC                         PIC  X(003).
             88 PO-DRPA1454-RC-OK                         VALUE 'OK '.

          05 PO-DRPA1454-HIS-KZ                     PIC  X(001).
             88 PO-DRPA1454-PEG-AKTUELL                  VALUE 'N'.
             88 PO-DRPA1454-PEG-HISTORISCH               VALUE 'J'.
          05 PO-DRPA1454-PMPE-KENNUNG               PIC  X(008).
          05 PO-DRPA1454-PMPE-LAST-UPDATE           PIC  X(026).
260603    05 P0-DRPA1454-PMPG-VERSION               PIC S9(004) BINARY.
      ******* ENDE COPY-MEMBER **************************************
