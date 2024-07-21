      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0283.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0283.CPv  $
      *
      *   Rev 2.3   Feb 25 2016 10:11:48   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:56:56   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:05:06   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 08 2015 11:16:14   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0283  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0283                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT MIT PIN <<<  TABELLE AGI TBAN0021             *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0283.
           05 PI-DRAN0283-AGI-KEY.
               10 PI-DRAN0283-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0283-AGI-PIN         PIC S9(9) COMP.
      *
       01  PO-DRAN0283.
           05 PO-DRAN0283-RC                   PIC  X(003).
             88 PO-DRAN0283-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0283-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0283-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
      *   AGI-DATEN AUS PC-DATEN
           COPY DRAN0284.
      *
      ****** ENDE COPY-MEMBER ****************************************
 


