      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM
      *            DATUM      :  28.10.2008
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2008 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0444.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0444.CPv  $
      *
      *   Rev 2.1   Nov 09 2009 10:18:14   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   OCT 28 2008 10:56:46   I13AZ23
      *ARCHIVERSTELLUNG
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0170  KONSTANTEN FÜR                     *
      *                           HAFTZEITTYP              KTY = 165 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OBER                                            *
      *    DATUM:    27.10.2008                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00820!27.10.08! 01 ! NEUERSTELLUNG             ! OBER     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-HAFTZEITTYP-KEINE       PIC S9(004) BINARY VALUE 1.
           05 K-KNR-HAFTZEITTYP-FIX         PIC S9(004) BINARY VALUE 2.
           05 K-KNR-HAFTZEITTYP-MAXFIX      PIC S9(004) BINARY VALUE 3.
           05 K-KNR-HAFTZEITTYP-VARIABEL    PIC S9(004) BINARY VALUE 4.
