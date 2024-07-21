      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0334.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0334.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:23:10   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:48:06   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 09:07:14   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Jan 28 2000 10:25:48   T13EX22
      *OPPA0064: Erweiterungen für Währungsprüfung
      *
      *   Rev 2.0   Feb 27 1999 14:28:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:51:20   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:21:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0334                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. GES/SPERRKRIT. >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    12.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.06.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!30.03.98! 1.1! RC-NOK EINGEFÜGT          ! WOLFF    *
      * APA00082!26.01.00! 2.1! WÄHRUNGSPRÜFUNG PEG       ! MUND     *
      * APA00622!22.05.07! 2.2! SCHALTER PO-S-VGES HINZU- ! STORCH   *
      *         !        !    ! GEFÜGT                    !          *
      * APA01101!23.02.12! 2.3! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0334                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0334.
          05 PI-DRPA0334-DATEN.
             10 PI-DRPA0334-TAGESDATUM-EUR          PIC  X(010).
             10 PI-DRPA0334-VORSATZ                 PIC  X(200).
      *
       01 PO-DRPA0334.
          05 PO-DRPA0334-RC                         PIC  X(003).
             88 PO-DRPA0334-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0334-RC-NOK                        VALUE 'NOK'.
          05 PO-DRPA0334-DATEN.
             10 PO-DRPA0334-PGEB-GES                PIC S9(004) BINARY.
             10 PO-DRPA0334-ABSENDER                PIC  X(030).
             10 PO-DRPA0334-EMPFAENGER              PIC  X(030).
             10 PO-DRPA0334-INHALT                  PIC  X(030).
             10 PO-DRPA0334-ERSTELL-DAT             PIC  X(010).
             10 PO-DRPA0334-WAEHRUNG                PIC  X(003).
             10 PO-DRPA0334-PDSP-ANZ-FEHL           PIC S9(009) BINARY.
             10 PO-DRPA0334-PDSP-PSATZ-FEHL         PIC S9(003)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0334-S-MANDANT               PIC  X(001).
      *
      ******* ENDE COPY-MEMBER **************************************
