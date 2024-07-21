      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1577.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1577.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 12:03:16   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   May 19 2003 10:00:48   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1577  FUNKTION LESE-OZP                  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
040903* AKI00114!04.09.03!2.5 ! Funktion DRKI1577         ! P. Pohl  *
040903*         !        !    ! (alle Verträge zu einer   !          *
040903*         !        !    ! ADRESSEID)                !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI1577.
          05  PI-DRKI1577-PROGRAMM                 PIC  X(08).
          05  PI-DRKI1577-UFKT-CODE                PIC  9(02).
          05  PI-DRKI1577-VRBPABADRESSEID          PIC  X(13).
          05  PI-DRKI1577-VRBPABPERSONENID         PIC X(13).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI1577.
           05  PO-DRKI1577-RC                  PIC  X(03).
               88  PO-DRKI1577-RC-OK           VALUE 'OK '.
               88  PO-DRKI1577-RC-NFD          VALUE 'NFD'.
               88  PO-DRKI1577-RC-EOF          VALUE 'EOF'.
               88  PO-DRKI1577-RC-LER          VALUE 'LER'.
           05  PO-DRKI1577-DATEN.
             10 PO-DRKI1577-VRBPERSONENID      PIC X(13).
             10 PO-DRKI1577-VRBPERSONENROLLEKZ PIC X(01).
             10 PO-DRKI1577-VRBVERTRAGSID      PIC X(13).
      *
      ******** ENDE COPY-MEMBER (DRKI1577) *******************

