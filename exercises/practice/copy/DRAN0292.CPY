      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0292.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0292.CPv  $
      *
      *   Rev 2.6   Feb 25 2016 10:13:18   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.5   Feb 12 2016 07:58:06   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.4   Feb 11 2016 16:49:10   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.3   Jan 22 2016 09:17:58   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation +
      * CKV
      *
      *   Rev 2.2   Jan 22 2016 09:10:46   t13ae34
      *AAN01238: Elektra-Anforderungen R. 7.28 - CKV
      *
      *   Rev 2.1   Aug 26 2015 12:06:14   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 10 2015 12:59:12   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0292  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!26.08.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0292                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT MIT VORSCHLAG <<<  TABELLE CKV TBAN0023       *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0292.
           05 PI-DRAN0292-CKV-KEY.
               10 PI-DRAN0292-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0292-CKV-HVANTNUM    PIC X(9).
      *
       01  PO-DRAN0292.
           05 PO-DRAN0292-RC                   PIC  X(003).
             88 PO-DRAN0292-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0292-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0292-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
      *   CKV-DATEN AUS PC-DATEN
           COPY DRAN0294.
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 
 
 


