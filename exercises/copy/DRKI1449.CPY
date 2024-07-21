      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1449.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1449.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 14:59:26   t13ex27
      *AKI00303 - KoKÜ-Vorschlagsermittlung
      *
      *   Rev 2.0   Sep 12 2002 08:29:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1449  FUNKTION LESE-OZP                  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00303!27.09.02!2.1 ! neu                       !I.Frenzel *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI1449.
          05  PI-DRKI1449-PROGRAMM                 PIC  X(008).
          05  PI-DRKI1449-UFKT-CODE                PIC  9(002).
          05  PI-DRKI1449-VRBPERSONENID            PIC X(013).
          05  PI-DRKI1449-VRBPERSONENROLLEKZ       PIC X(001).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI1449.
           05  PO-DRKI1449-RC                  PIC  X(03).
               88  PO-DRKI1449-RC-OK           VALUE 'OK '.
               88  PO-DRKI1449-RC-NFD          VALUE 'NFD'.
               88  PO-DRKI1449-RC-EOF          VALUE 'EOF'.
               88  PO-DRKI1449-RC-LER          VALUE 'LER'.
           05  PO-DRKI1449-DATEN.
             10 PO-DRKI1449-VRBVERTRAGSID      PIC X(13).
      *
      ******** ENDE COPY-MEMBER (DRKI1449) *******************
