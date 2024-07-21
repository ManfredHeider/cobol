      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1644.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1644.CPv  $
      *
      *   Rev 2.1   Feb 19 2004 11:43:24   t13ex49
      *bh:\mfe\cpy\DRKI1644.CPY -> L:\mf\CPY\DRKI1644.CPv
      *Enter description of changes.
      *erstmalige Übergabe
      *
      *   Rev 2.0   Feb 17 2004 10:14:16   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1644                                      *
      *    --------------------------------------------------------   *
      *    Kontrolldatei für AdressArtMigration Rückwärts             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Peter Pohl                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00114!10.02.04!    ! NEUERSTELLUNG             !Peter Pohl*
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      ****************************************************************
      *
       01 DRKI1644.
          05 DRKI1644-TEXT1                 PIC  X(010).
          05 DRKI1644-Adresse-PLZ           PIC  X(015).
          05 DRKI1644-T-ADRESSEPLZ          PIC  X(015).
          05 DRKI1644-Adresse-ORT           PIC  X(030).
          05 DRKI1644-T-ADRESSEORT          PIC  X(030).
          05 DRKI1644-Adresse-STR           PIC  X(045).
          05 DRKI1644-T-AdresseSTR          PIC  X(045).
          05 DRKI1644-RESERVE               PIC  X(050).
          05 DRKI1644-ID REDEFINES DRKI1644-RESERVE.
               10  DRKI1644-PERSONENID      PIC  X(013).
               10  Filler                   PIC  X(037).
      ***** Ende Copy-Member DRKI1644 *****************************

