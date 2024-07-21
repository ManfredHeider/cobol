      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2001.CPv  $
      *
      *   Rev 2.1   Dec 20 2006 15:55:44   t13ex43
      *FKI00979: Fremdverträge mit mehreren VN
      *
      *   Rev 2.0   Dec 13 2006 08:29:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2001  FUNKTION LESE-OZP                  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00556!13.12.06!2.1 ! neu                       !I.Frenzel *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI2001.
          05  PI-DRKI2001-PROGRAMM                 PIC  X(008).
          05  PI-DRKI2001-UFKT-CODE                PIC  9(002).
          05  PI-DRKI2001-VRBPERSONENID            PIC X(013).
          05  PI-DRKI2001-VRBPERSONENROLLEKZ       PIC X(001).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI2001.
           05  PO-DRKI2001-RC                  PIC  X(03).
               88  PO-DRKI2001-RC-OK           VALUE 'OK '.
               88  PO-DRKI2001-RC-NFD          VALUE 'NFD'.
               88  PO-DRKI2001-RC-EOF          VALUE 'EOF'.
               88  PO-DRKI2001-RC-LER          VALUE 'LER'.
           05  PO-DRKI2001-DATEN.
             10 PO-DRKI2001-VRBVERTRAGSID      PIC X(13).
      *
      ******** ENDE COPY-MEMBER (DRKI2001) *******************
