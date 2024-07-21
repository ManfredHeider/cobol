      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1421.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1421.CPv  $
      *
      *   Rev 2.2   Nov 03 2006 13:29:36   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   15 Sep 2000 09:24:40   T13EX21
      *APA00098 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1421  FUNKTION PRODUKT/VERTRAGSNUMMER    *
      *                                    FORMAL PÜFEN              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    07.09.2000                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00098!07.09.00!2.1  ! NEUERSTELLUNG             ! STORCH  *
      * APA00452!03.11.06!2.2    Umstellung 3-stell. Prod. ! Jäger   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA1421.
          05 PI-DRPA1421-PGEB-GES                PIC S9(004) BINARY.
          05 PI-DRPA1421-EXT-VNR                 PIC  X(015).
031106*   05 PI-DRPA1421-PRODUKT                 PIC S9(004) BINARY.
031106    05 PI-DRPA1421-PROD-PROD-SCHL          PIC S9(004) BINARY.
      *
       01 PO-DRPA1421.
          05 PO-DRPA1421-RC                     PIC  X(003).
             88 PO-DRPA1421-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1421-RC-UNZULAESSIG               VALUE 'UNZ'.
      *
      ****** ENDE COPY-MEMBER ****************************************
