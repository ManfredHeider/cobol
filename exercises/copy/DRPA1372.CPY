      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1372.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1372.CPv  $
      *
      *   Rev 2.5   Jun 22 2018 09:11:00   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.4   Apr 30 2009 09:24:42   t13ex44
      *APA00852: Ges. 92 - Vertragsnr.
      *
      *   Rev 2.3   19 Dec 2000 14:58:52   T13EX43
      *APA00111 : Wiederholung NTPUT (identisch mit Vorversion)
      *
      *   Rev 2.2   02 Nov 2000 13:32:26   T13EX43
      *APA00112 : DIT-PEG umsetzen und einführen
      *
      *   Rev 2.1   May 04 2000 11:51:00   T13EX23
      *APA00067 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1372  FUNKTION DAISY EXTERNE VNR IN      *
      *                                    FREMD-FORMAT KONVERTIEREN *
      *    -------------------------------------------------------   *
      *    ERSTELLT: GRAY                                            *
      *    DATUM:    18.04.2000                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00067!18.04.00!2.1  ! NEUERSTELLUNG             ! GRAY    *
      * APA00112!31.10.00!2.2  ! RC-UNZ HINZU              !KLEEMEIER*
      * APA00852!24.04.09!2.3  ! RC-NOT-NUM HINZU          ! SCHEIDEL*
      * APA02579!21.06.18!2.5  ! HERKUNFT HINZU            ! SCHEIDEL*
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA1372.
          05 PI-DRPA1372-PGEB-GES                PIC S9(004) BINARY.
          05 PI-DRPA1372-DSY-EXT-VNR             PIC  X(015).
          05 PI-DRPA1372-KNR-FREMDSYSTEM         PIC S9(004) BINARY.
210618    05 PI-DRPA1372-KNR-BEW-HRKFT           PIC S9(004) BINARY.
      *
       01 PO-DRPA1372.
          05 PO-DRPA1372-RC                      PIC  X(003).
             88 PO-DRPA1372-RC-OK                        VALUE 'OK '.
311000       88 PO-DRPA1372-RC-UNZ                       VALUE 'UNZ'.
240409       88 PO-DRPA1372-RC-NOT-NUM                   VALUE 'NNU'.
          05 PO-DRPA1372-EXT-VNR-ALPHA           PIC  X(015).
          05 PO-DRPA1372-EXT-VNR-NUMERISCH       PIC  9(015).
      ****** ENDE COPY-MEMBER ****************************************

