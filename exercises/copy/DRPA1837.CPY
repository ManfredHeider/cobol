      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.08.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1837.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1837.CPv  $
      *
      *   Rev 2.1   Aug 12 2015 12:24:26   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: MSC
      *
      *   Rev 2.0   Aug 11 2015 11:34:54   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1837                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1837 DES UPPA0140       *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * APA01932!11.08.15! 2.0! NEU                       ! SCHEIDEL  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FUER DIE FUNKTION DRPA1837. MIT DIESER FUNKTION WIRD EINE
      *    KORREKTUR DES UMSATZSTEUERLICHEN STATUS EINES VBS
      *    ANGESTOSSEN.
      *
      *****************************************************************
      *
        01 PI-DRPA1837.
           05 PI-DRPA1837-VBNR              PIC S9(009) BINARY.
           05 PI-DRPA1837-ZIEL-STATUS       PIC S9(004) BINARY.
           05 PI-DRPA1837-JAHR              PIC S9(004) BINARY.
           05 PI-DRPA1837-VB-AUFGABE        PIC  X(026).
      *
        01 PO-DRPA1837.
           05 PO-DRPA1837-RC                PIC  X(003).
              88 PO-DRPA1837-RC-OK               VALUE 'OK '.
              88 PO-DRPA1837-RC-NOT-OK-SEL       VALUE 'NOS'.
              88 PO-DRPA1837-RC-NOT-OK-INS       VALUE 'NOI'.
      *
           05 PO-DRPA1837-SQLCODE           PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************

