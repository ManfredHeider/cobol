      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0114.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0114.CPv  $
      *
      *   Rev 2.7   Jun 09 2020 16:10:58   t13ae34
      *AAN01476: Neue Satzart 4 für AMLeben-Tarife RN1, RFN oder RFPN
      *
      *   Rev 2.6   Jun 26 2014 08:15:14   t13ae34
      *AAN01168: VERMÖGENS- & EINKOMMENSPLAN - VEP
      *
      *   Rev 2.5   Dec 09 2013 14:40:48   t13ae34
      *AAN01139: neuer Pensionskassentarif PRGV
      *
      *   Rev 2.4   Jun 10 2013 14:41:54   t13ae23
      *AAN01104: Konsumbonus Erweiterung Datei AML PZM-Daten um KZ
      *Konsumbonus
      *
      *   Rev 2.3   Dec 10 2010 13:00:50   t13ae34
      *AAN00977: Anpassung der Schnittstellen und Ressourcen
      *
      *   Rev 2.2   19 Jul 2007 09:41:18   t13ae23
      *AAN00762: Kennzeichen Vertriebsgesellschaft
      *
      *   Rev 2.1   08 Sep 2006 09:41:06   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.0   10 Jul 2006 15:05:22   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                               *
      *  SATZBESCHREIBUNG TÄGLICHER FILETRANSFER DVAG-AACHEN          *
      *                                                               *
      *                                                               *
      *****************************************************************
      * ERSTELLT AM : 10.07.2006                                      *
      *         VON : V. RAHNER                                       *
      *                                                               *
      * LOGBUCH                                                       *
      * NEU IM RAHMEN UMSTELLUNG AM-LEBEN AUF BLACK-BOX VERFAHREN     *
      * ERSETZT DRAN0028                                              *
      *                                                               *
      * AUFTRAG ! DATUM  ! VER! ÄENDERUNG                 ! NAME      *
      * --------+--------+--------------------------------+-----------*
      * AAN00685!10.07.06!2.1 ! NEU                       ! RAHNER    *
      * AAN00730!23.07.07!2.2 ! ALLFINANZ                 ! RAHNER    *
      * AAN00977!08.12.10!2.3 ! SATZLÄNGE                 ! HEIDER    *
      * AAN01104!10.06.13!2.4 ! KENNZ. KONSUMBONUS+FILLER ! RAHNER    *
      * AAN01139!06.12.13!2.5 ! XGDV VERGRÖSSERN AUF 5000 ! HEIDER    *
      * AAN01168!24.04.14!2.6 !VEP, XGDV -> 20.000        ! HEIDER    *
      * AAN01476!08.06.20!2.7 ! NEUE SATZART 4            ! HEIDER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
       01  ANDATTRANS.
           03  PZM-FT-DVAG-TTRANS-OHNE-PC.
           05  PZM-FT-DVAG-SATZART                PIC  X.
           88  PZM-FT-DVAG-OHNE-PC-DATEN          VALUE '1'.
           88  PZM-FT-DVAG-MIT-PC-DATEN           VALUE '2'.
080620*        MANUELLE ANTRÄGE MIT BESTIMMTEN TARIFEN
080620     88  PZM-FT-DVAG-OHNE-PC-SATZART-4      VALUE '4'.
      *
      *    RUMPFDATEN
           05  PZM-FT-DVAG-AML-SPARTE             PIC  X.
           05  PZM-FT-DVAG-AML-VSNR               PIC  X(7).
           05  PZM-FT-DVAG-AML-VSNR-9
               REDEFINES PZM-FT-DVAG-AML-VSNR     PIC  9(7).
230707     05  PZM-FT-DVAG-AML-VBGES              PIC  X(2).
           05  PZM-FT-DVAG-AML-ORGGS              PIC  X(3).
           05  PZM-FT-DVAG-AML-AGT                PIC  X(4).
           05  PZM-FT-DVAG-AML-DIREKT             PIC  X(3).
           05  PZM-FT-DVAG-AML-EIGENANTRAG        PIC  X.
100613     05  PZM-FT-DVAG-AML-KONSUMBONUS        PIC  X.
100613     05  PZM-FT-DVAG-AML-FILLER             PIC  X(99).
      *    BLACK-BOX
240414*    05  PZM-FT-DVAG-AML-XGDV               PIC  X(5000).
240414     05  PZM-FT-DVAG-AML-XGDV               PIC  X(20000).


