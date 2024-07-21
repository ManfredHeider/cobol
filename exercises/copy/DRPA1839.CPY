      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1839.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1839.CPv  $
      *
      *   Rev 2.2   Nov 17 2015 10:17:22   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      * PA04
      *
      *   Rev 2.1   Oct 19 2015 10:07:46   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      * PA04
      *
      *   Rev 2.0   Oct 16 2015 14:33:38   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1839                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1839                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * APA01932!28.07.15! 2.1! NEU                       ! STORCH    *
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
      *    FUNKTION DRPA1839:
      *    LESEN HÖCHSTE(AKTUELLE) PASU-LFDNR ZU EINER PASU-VBNR
      *
      *****************************************************************
      *
       01  PI-DRPA1839.
           05  PI-DRPA1839-PASU-VBNR          PIC S9(009) BINARY.
           05  PI-DRPA1839-PASU-JAHR          PIC S9(004) BINARY.
           05  PI-DRPA1839-PASU-LFDNR         PIC S9(004) BINARY.

      *
       01  PO-DRPA1839.
           05  PO-DRPA1839-RC                 PIC  X(003).
               88 PO-DRPA1839-RC-OK                  VALUE 'OK '.
               88 PO-DRPA1839-RC-NOT-FOUND           VALUE 'NFD'.
      *
           05  PO-DRPA1839-PASU-ZUSTAND       PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************


