      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1576.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1576.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 12:03:12   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   May 19 2003 10:00:42   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1576  FUNKTION LESE-OZP                  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
040903* AKI00114!04.09.03!2.5 ! Funktion DRKI1576         ! P. Pohl  *
040903*         !        !    ! (alle Verträge zu einer   !          *
040903*         !        !    ! ADRESSEID)                !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI1576.
          05  PI-DRKI1576-PROGRAMM                 PIC  X(08).
          05  PI-DRKI1576-UFKT-CODE                PIC  9(02).
          05  PI-DRKI1576-VRBPABADRESSEID             PIC  X(13).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI1576.
           05  PO-DRKI1576-RC                  PIC  X(03).
               88  PO-DRKI1576-RC-OK           VALUE 'OK '.
               88  PO-DRKI1576-RC-NFD          VALUE 'NFD'.
               88  PO-DRKI1576-RC-EOF          VALUE 'EOF'.
               88  PO-DRKI1576-RC-LER          VALUE 'LER'.
           05  PO-DRKI1576-DATEN.
             10 PO-DRKI1576-VRBPERSONENID      PIC X(13).
             10 PO-DRKI1576-VRBPERSONENROLLEKZ PIC X(01).
             10 PO-DRKI1576-VRBVERTRAGSID      PIC X(13).
      *
      ******** ENDE COPY-MEMBER (DRKI1576) *******************

