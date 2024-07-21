      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1211.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1211.CPv  $
      *
      *   Rev 2.1   26 Jun 2001 08:13:18   t13ex13
      *AKI00165 - PC-Nummern löschen
      *
      *   Rev 2.0   21 Mar 2001 14:10:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00165!23.03.01!2.0 ! NEU                       ! A.Blaess *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  PI-DRKI1211.
           05 PI-DRKI1211-PROGRAMM              PIC  X(008).
           05 PI-DRKI1211-UFKT                  PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI1211-KEY.
              10 PI-DRKI1211-ERSTELLERID        PIC  X(013).
      *
       01  PO-DRKI1211.
           05 PO-DRKI1211-RC                    PIC  X(003).
              88 PO-DRKI1211-RC-OK                   VALUE 'OK '.
              88 PO-DRKI1211-RC-NOT-FOUND            VALUE 'NFD'.
              88 PO-DRKI1211-RC-EOF                  VALUE 'EOF'.
      *
           05 PO-DRKI1211-DATEN.
              10 PO-DRKI1211-DOKUMENTID         PIC X(013).
              10 PO-DRKI1211-ERSTELLDATUM       PIC X(010).
              10 PO-DRKI1211-ERSTELLERROLLE     PIC X(001).
              10 PO-DRKI1211-SUBTYPEDOKUMENT    PIC X(004).
              10 PO-DRKI1211-LETZTEAENDERUNG    PIC X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
