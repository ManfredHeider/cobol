      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0312.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0312.cpv  $
      *
      *   Rev 2.1   Aug 23 1999 11:43:34   T13EX21
      *APA00049 - PO-Daten definiert
      *
      *   Rev 2.0   Feb 27 1999 14:28:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:44:10   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:34:34   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0312  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OPRITZ                                          *
      *    DATUM:    05.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.06.97! 01  ! NEUERSTELLUNG            ! OPRITZ   *
      *         !        !     !                          !          *
      * APA00012!05.03.98!01.02! PMPG-VERSION ENTFERNT    ! JÄGER    *
      *         !        !     !                          !          *
      * APA00012!08.07.99!01.03! PO-DATEN DEFINIERT       ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0312                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL<       TABELLE PMPG TBPA0100              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0312.
          05 PI-DRPA0312-PMPG-KEY.
             10 PI-DRPA0312-PMPG-ID           PIC S9(015) COMP-3.
050398*      10 PI-DRPA0312-PMPG-VERSION      PIC S9(004) COMP.
      *
       01 PO-DRPA0312.
          05 PO-DRPA0312-RC                   PIC  X(003).
             88 PO-DRPA0312-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0312-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0312-RC-NOT-FOUND                  VALUE 'NFD'.
      *** RC-CHANGED WIRD IN DIESER FKT. NICHT GESETZT. DA ABER PRODUK-
      *** TIVE PROGRAMME GEÄNDERT WERDEN MÜSSTEN, HABE ICH ES ZUNÄCHST
      *** IM PO-BEREICH STEHENGELASSEN (MST, 08.07.1999).
      *
080799    05 PO-DRPA0312-DATEN.
050398       10 PO-DRPA0312-PMPG-VERSION         PIC S9(004) COMP.
             10 PO-DRPA0312-PDTA-ID              PIC S9(015) COMP-3.
             10 PO-DRPA0312-PMPG-LFD-NR-VNR      PIC S9(009) COMP.
             10 PO-DRPA0312-PMPG-VSTAT           PIC  X(004).
             10 PO-DRPA0312-PMPG-LAST-UPDATE     PIC  X(026).
             10 PO-DRPA0312-PMPG-KENNUNG         PIC  X(008).
             10 PO-DRPA0312-PMPG-INHALT.
                15 PO-DRPA0312-PMPG-INHALT-LEN   PIC S9(004) COMP.
                15 PO-DRPA0312-PMPG-INHALT-TEXT  PIC X(3900).
      *
      ****** Ende Copy-Member ****************************************
