      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1112.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1112.CPv  $
      *
      *   Rev 2.2   Jan 19 2000 08:48:44   T13EX21
      *APA00063 - DTA-Sachbearbeiter
      *
      *   Rev 2.1   Mar 10 1999 14:45:22   T13AE41
      *Neuer FC NPM-AUF-PMBE
      *
      *   Rev 1.0   21 Oct 1998 13:57:24   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1112                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<FKPW066 - PVW-STEUERUNG ONLINE>>         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: LÜTTGES                                          *
      *    DATUM:    04.11.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.11.98! 01 ! NEU                       ! LÜTTGES  *
      * AFPA0012!09.03.99! 02 ! NEUER RC NPM-AUF-PMBE     ! LÜTTGES  *
      * APA00063!13.10.99! 03 ! PO-BEREICH ERWEITERT      ! STORCH   *
      *         !        !    ! RC NPM-AUF-PMBE ENFERNT   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1112                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1112.
          05 PI-DRPA1112-PMPG-ID              PIC S9(15) PACKED-DECIMAL.
          05 PI-DRPA1112-PMPG-VERSION         PIC S9(004) BINARY.
      *
       01 PO-DRPA1112.
          05 PO-DRPA1112-RC                   PIC  X(003).
             88 PO-DRPA1112-RC-OK                  VALUE 'OK '.
             88 PO-DRPA1112-RC-NOK                 VALUE 'NOK'.
090399*      88 PO-DRPA1112-RC-NPM-AUF-PMBE        VALUE 'NAB'.
          05 PO-DRPA1112-VSTATUS              PIC  X(004).
131099    05 PO-DRPA1112-MELDUNG-TYP          PIC  X(001).
131099    05 PO-DRPA1112-MELDUNG-NR           PIC  9(003).

      ******* ENDE COPY-MEMBER **************************************
