      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1411.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1411.CPv  $
      *
      *   Rev 2.1   21 Sep 2000 16:06:48   T13EX21
      *APA00106 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:38   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER DRPA1411: DATENFELDER FÜR DIALOG PO14         *
      *    (PROVISIONSBESTAND - AUSKUNFT VERTRAGSNUMMERN)            *
      *    BENUTZT ALS PO-DRPA1408-ERGEBNISZEILE                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    24.07.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00106!24.07.00! 2.1! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIALOG PO14       *
      *    (PROVISIONSBESTAND - AUSKUNFT VERTRAGSNUMMERN)            *
      *    UNTERER MASKENTEIL                                        *
      *                                                              *
      ****************************************************************
      ***   DATENFELDER FÜR LISTDIALOG PO14

          30 PO-DRPA1411-ERGEBNISZEILE.
             35 PINZ-PIN                 PIC  9(009).
             35 PEW-KZ                   PIC  X(001).
             35 PINZ-PG-VBNR             PIC S9(007) COMP.
             35 PINZ-GUELTIG-AB          PIC  X(010).
             35 PINZ-KENNUNG             PIC  X(008).
             35 PINZ-LAST-UPDATE         PIC  X(026).
