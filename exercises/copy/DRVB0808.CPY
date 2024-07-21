      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0808.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0808.CPv  $
      *
      *   Rev 2.2   Mar 31 2006 13:10:28   t13ex13
      *AVD00026: Korrektur Einheiten abger. Geschäft
      *
      *   Rev 2.1   Feb 07 2006 13:40:40   t13ex13
      *AVD00026: Vergl. Umrechnung über VBI
      *
      *   Rev 2.0   Jan 26 2006 10:44:10   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0808                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR RELEVANTE EREIGNISSE                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: R.GUTFLEISCH                                     *
      *    DATUM:    26.01.2006                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00026!20.03.98! 2.0! NEU                       !GUTFLEISCH*
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  RELEVANTE-EREIGNISSE.
           05  RE-DIR            PIC S9(004) COMP.
           05  RE-VBNR           PIC S9(009) COMP.
           05  RE-STAT-DAT       PIC  X(006).
           05  RE-ERFASS-DAT     PIC  X(010).
      *        FORMAT TT.MM.JHJJ
           05  RE-KTY-REL-EREIG  PIC S9(004) COMP.
           05  RE-KNR-REL-EREIG  PIC S9(004) COMP.
           05  RE-VBNR-STRUK     PIC S9(009) COMP.
           05  RE-DIR-NEU        PIC S9(004) COMP.
      ******* ENDE COPY-MEMBER **************************************
