      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1038.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1038.CPv  $
      *
      *   Rev 2.7   Sep 26 2016 11:17:26   t13ex67
      *APA02280: Dialoganpassungen PF11-PF14
      *
      *   Rev 2.6   Aug 12 2015 11:27:52   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.5   Jul 06 2015 12:12:38   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12)
      *
      *   Rev 2.4   Jul 06 2015 11:50:10   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12)
      *
      *   Rev 2.3   Jun 19 2012 09:26:32   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:51:22   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:30:14   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 26 1999 13:42:52   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 14:00:22   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1038  FUNKTION PRÜFE PM AUF FOLGEFEHLER  *
      *    FUNKTION :   SUMMEN PRO VSTAT-KLASSEN                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    19.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!10.08.98!01.01! NEUERSTELLUNG             !SCHRIEBER*
      * APA01101!23.02.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01932!01.07.15!2.04! NEUE PO-FELDER FÜR LUCY   ! KÖPRÜCÜ  *
      * APA02280!20.06.16!2.05! PI-BEREICH ERWEITERT      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1038                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1038.
          05 PI-DRPA1038-PGEB-GES         PIC S9(009) BINARY.
          05 PI-DRPA1038-ABRECH-DATUM-VON PIC  X(010).
          05 PI-DRPA1038-ABRECH-DATUM-BIS PIC  X(010).
          05 PI-DRPA1038-VSTAT-KLASSE     PIC  X(001).
          05 PI-DRPA1038-MANDANT          PIC  9(02).
          05 PI-DRPA1038-S-MANDANT        PIC  X(01).
200616    05 PI-DRPA1038-PRODUKT          PIC  9(03).
200616    05 PI-DRPA1038-S-PRODUKT        PIC  X(01).
      *
       01 PO-DRPA1038.
          05 PO-DRPA1038-RC                PIC  X(003).
             88 PO-DRPA1038-RC-OK               VALUE 'OK '.
             88 PO-DRPA1038-RC-NOT-FOUND        VALUE 'NFD'.
          05 PO-DRPA1038-DATEN.
             10 PO-DRPA1038-ANZ-BEWEGUNGEN PIC S9(09)    BINARY.
             10 PO-DRPA1038-SUMME-GUTSCH   PIC S9(09)V99 PACKED-DECIMAL.
             10 PO-DRPA1038-SUMME-BELAST   PIC S9(09)V99 PACKED-DECIMAL.
             10 PO-DRPA1038-ABRECH-DATUM   PIC  X(10).
010715       10 PO-DRPA1038-SUMME-UST-BETR-GUT
010715                                     PIC S9(09)V99 PACKED-DECIMAL.
010715       10 PO-DRPA1038-SUMME-UST-BETR-BEL
010715                                     PIC S9(09)V99 PACKED-DECIMAL.
      *
      ****** Ende Copy-Member ****************************************


