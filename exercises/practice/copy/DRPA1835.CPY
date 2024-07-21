      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1835.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1835.CPv  $
      *
      *   Rev 2.1   Aug 12 2015 13:11:32   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.0   Jul 28 2015 08:38:16   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1835                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1834                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * APA01932!28.07.15! 2.0! NEU                       ! STRAUSS   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *
      *    DB2-LESE-OZP FÜR DIE TABELLE TBPA0035 PASU
      *    (OHNE SELECT AUF ALLE ATTRIBUTE)
      *
      *    FUNKTION DRPA1835:
      *    LESEN HÖCHSTE(AKTUELLE) PASU-LFDNR ZU EINER PASU-VBNR
      *
      *****************************************************************
      *
       01  PI-DRPA1835.
           05  PI-DRPA1835-PASU-VBNR          PIC S9(009) BINARY.
      *
       01  PO-DRPA1835.
           05  PO-DRPA1835-RC                 PIC  X(003).
               88 PO-DRPA1835-RC-OK                  VALUE 'OK '.
               88 PO-DRPA1835-RC-NOT-FOUND           VALUE 'NFD'.
      *
           05  PO-DRPA1835-PASU-LFDNR         PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************

