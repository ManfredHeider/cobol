      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0406.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0406.CPv  $
      *
      *   Rev 2.2   27 Apr 2006 13:56:36   t13ex22
      *AVB00764: Betreuerwechsel - Ermittlung für VBI-Bef.VS
      *
      *   Rev 2.1   Sep 23 2005 11:17:46   t13ex22
      *AVD00008: Projekt VBI: SST Beförderungsprüfung
      *
      *   Rev 2.0   Jun 17 2005 09:14:28   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KNR-KONSTANTEN FUER EREIGNIS
      *    ANFANG UND EREIGNIS ENDE (DRAT0406)          *
      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    17.06.2005                                       *
      *****************************************************************
      *    VERSION   1    17.06.05 11:40:25
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00008!17.06.05! 01 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
      *   AUSPRÄGUNGEN EREIGNIS-ANFANG; KTY = 211
      *
       01  K-KNR-EANF-SCHON-VORHER-UNTST    PIC S9(04) COMP VALUE 0.
       01  K-KNR-EANF-NEUAUFN-FREMD-STRUK   PIC S9(04) COMP VALUE 1.
       01  K-KNR-EANF-NEUAUFN-UEBERH        PIC S9(04) COMP VALUE 2.
       01  K-KNR-EANF-STRUKTUR-ALT          PIC S9(04) COMP VALUE 3.

      *   AUSPRÄGUNGEN EREIGNIS-ENDE;   KTY = 212
      *
       01  K-KNR-EENDE-IMMER-NOCH-UNTST     PIC S9(04) COMP VALUE 0.
       01  K-KNR-EENDE-AUSGESCHIEDEN        PIC S9(04) COMP VALUE 1.
       01  K-KNR-EENDE-VERL-DURCH-UEBERH    PIC S9(04) COMP VALUE 2.
       01  K-KNR-EENDE-RUECKSTUFUNG-VM      PIC S9(04) COMP VALUE 3.
       01  K-KNR-EENDE-VERL-DURCH-ANTRAG    PIC S9(04) COMP VALUE 4.

