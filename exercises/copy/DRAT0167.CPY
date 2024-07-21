      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 Atlas Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0167.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAT0167.CPv  $
      *
      *   Rev 2.2   Jun 29 1999 10:18:00   T13AE34
      *FRZ00028-PVCS Korrektur
      *
      *   Rev 2.1   Jun 22 1999 23:05:44   T13AE18
      *apa00012
      *
      *   Rev 1.0   05 SEP 1997 09:20:24   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0167  KONSTANTEN FÜR                     *
      *                           ENTSTEHUNG-KENNZEICHEN   KTY = 134 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.10.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA00??!22.10.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      * AFPA0012!03.08.98! 02 ! '5' FUER MIGRATION        ! SCHEIDEL *
      * AFPA0012!28.10.98! 03 ! '6' VM IN PES EINGEFüGT   !BOHLINGER *
      * AFPA0012!21.12.98! 04 ! '7' PE-STURKTUR WURDE MAN.!BOHLINGER *
      *         !        !    ! IM EVS-UMB-DIALOG ERSTELLT!          *
      *         !        !    !                           !          *
      * AFPA0012!21.06.99!2.01! '8' PE-Struktur wurde man.!Bohlinger *
      *         !        !    ! IM UMB-DIALOG durch änd.  !          *
      *         !        !    ! einer bestehenden pe-struk!          *
      *         !        !    ! erstellt                  !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PES-PVT              PIC S9(004) BINARY VALUE 1.
           05 K-KNR-PES-PEW              PIC S9(004) BINARY VALUE 2.
           05 K-KNR-PES-MANUELL          PIC S9(004) BINARY VALUE 3.
           05 K-KNR-PES-VORSTRUK         PIC S9(004) BINARY VALUE 4.
           05 K-KNR-PES-MIGRATION        PIC S9(004) BINARY VALUE 5.
           05 K-KNR-PES-VM-AEND          PIC S9(004) BINARY VALUE 6.
           05 K-KNR-PES-EVS-MAN-AEND     PIC S9(004) BINARY VALUE 7.
           05 K-KNR-PES-UMB-MAN-AEND     PIC S9(004) BINARY VALUE 8.
