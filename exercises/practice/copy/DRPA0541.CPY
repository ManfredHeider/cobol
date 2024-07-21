      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0541.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0541.cpv  $
      *
      *   Rev 2.1   Jun 09 1999 13:31:36   T01JM2S
      *APA00012 - UMSETZUNG FACHL. ANFORDERUNG FKPAG026
      *
      *   Rev 2.0   Feb 27 1999 14:29:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:03:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:29:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0541                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<DATEN SPEICHERN NACH äNDERUNGEN IN PEG>> *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    01.10.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!01.10.97! 1.0! NEU                       !SCHRIEBER *
      * AFPA0012!30.10.98! 1.1! RC = CHA EINFÜGEN         !SCHRIEBER *
      *         !        !    !                           !          *
      * APA00012!07.06.99!2.1 ! ÄNDERUNG WEGEN NEUER      ! WOLFF    *
      *         !        !    ! FUNKTION DRPA1257         !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0541                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0541.
070699*   05 PI-DRPA0541-DATEN-ALT.
070699*      10 PI-DRPA0541-UFKT                    PIC  9(002).
070699*      10 PI-DRPA0541-PMPG-ID                 PIC S9(015) COMP-3.
070699*      10 PI-DRPA0541-PMPG-VERSION            PIC S9(004) COMP.
070699*      10 PI-DRPA0541-PDTA-ID                 PIC S9(015) COMP-3.
070699*      10 PI-DRPA0541-PMPG-LFD-NR-VNR         PIC S9(004) COMP.
070699*      10 PI-DRPA0541-PMPG-VSTAT              PIC  X(004).
070699*      10 PI-DRPA0541-PMPG-INHALT-LEN         PIC S9(004) COMP.
070699*      10 PI-DRPA0541-PMPG-INHALT             PIC X(3900).
070699*      10 PI-DRPA0541-PMPG-LAST-UPDATE        PIC X(26).
070699*      10 PI-DRPA0541-PMPG-KENNUNG            PIC X(08).
070699    05 PI-DRPA0541-PMPG-ALT.
070699       COPY DRPA1258.

      *
070699*   05 PI-DRPA0541-DATEN-NEU.
070699*      10 PI-DRPA0541-UFKT                    PIC  9(002).
070699*      10 PI-DRPA0541-PMPG-ID                 PIC S9(015) COMP-3.
070699*      10 PI-DRPA0541-PMPG-VERSION            PIC S9(004) COMP.
070699*      10 PI-DRPA0541-PDTA-ID                 PIC S9(015) COMP-3.
070699*      10 PI-DRPA0541-PMPG-LFD-NR-VNR         PIC S9(004) COMP.
070699*      10 PI-DRPA0541-PMPG-VSTAT              PIC  X(004).
070699*      10 PI-DRPA0541-PMPG-INHALT-LEN         PIC S9(004) COMP.
070699*      10 PI-DRPA0541-PMPG-INHALT             PIC X(3900).
070699*      10 PI-DRPA0541-PMPG-LAST-UPDATE        PIC X(26).
070699*      10 PI-DRPA0541-PMPG-KENNUNG            PIC X(08).
070699    05 PI-DRPA0541-PMPG-NEU.
070699       COPY DRPA1258.
      *
       01 PO-DRPA0541.
          05 PO-DRPA0541-RC                         PIC  X(003).
             88 PO-DRPA0541-RC-OK                         VALUE 'OK '.
070699*      88 PO-DRPA0541-RC-NOT-OK                     VALUE 'NOK'.
             88 PO-DRPA0541-RC-CHANGED                    VALUE 'CHA'.
070699*   05 PO-DRPA0541-FEHLER-ORT                 PIC  9(001).
      *
      ******* ENDE COPY-MEMBER **************************************
