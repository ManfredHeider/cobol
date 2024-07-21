      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1755.CPY  $
 
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1755.CPv  $
      *
      *   Rev 2.5   Jun 09 2018 12:56:34   t13ex44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.4   Sep 01 2014 10:40:54   t13ae39
      *APA01827: Haftungszeitreduzierung Central / neuer PGV
      *
      *   Rev 2.3   Mar 25 2014 13:06:44   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g):
      *
      *   Rev 2.2   Jan 10 2014 13:52:32   t13ex72
      *APA01542: Erstellung Gevo Neutral
      *
      *   Rev 2.1   Aug 14 2013 15:15:12   t13ex70
      *APA01542: Erstellung Gevo Neutral
      *
      *   Rev 2.0   Aug 14 2013 14:16:44   t13ex70
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1755                                      *
      *    --------------------------------------------------------   *
      *    KONSTANTENTABELLE                                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WEINMANN                                         *
      *    DATUM:    14.08.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01542!14.08.13! 01 ! NEU                       ! WEINMANN *
      * APA01550!17.01.14!2.3 ! NEUE PGV'S AML            ! BÖTTCHER *
      * APA01827!11.08.14!2.4 ! Neu: CKV GEVO-NEUTRAL 4065! Strauss  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESE TABELLE ENTHÄLT EINE LISTE DER PGVs DIE EINE        *
      *    NEUTRALE BEWEGUNG (GEVO-NEUTRAL) DARSTELLEN               *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1755.
           05  DRPA1755-NEUTRALEN-PGV-PRUEFEN.
               10 DRPA1755-GES           PIC  9(02).
               10 DRPA1755-PGV           PIC  9(04).
           05  DRPA1755-ALLE-PGV-NEUTRAL REDEFINES
               DRPA1755-NEUTRALEN-PGV-PRUEFEN
                                         PIC  9(06).
               88 S-PGV-IST-NEUTRAL      VALUE
                                         201001
                                         201002
                                         201003
                                         201004
                                         201005
                                         201006
                                         201007
                                         201008
                                         201022
                                         201033
                                         201044
                                         201055
170114                                   201099
 
                                         202001
                                         202002
                                         202003
                                         202004
                                         202005
                                         202006
                                         202007
                                         202008
                                         202022
                                         202033
                                         202044
                                         202055
 
                                         203001
                                         203002
                                         203003
                                         203004
                                         203005
                                         203006
                                         203007
                                         203008
                                         203022
                                         203033
                                         203044
                                         203055
 
                                         204001
                                         204002
                                         204003
                                         204004
                                         204005
                                         204006
                                         204007
                                         204008
                                         204022
                                         204033
                                         204044
                                         204055
170114                                   204099
 
240418                                   211009
 
110814                                   504065
                                         .

