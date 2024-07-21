      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0282.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0282.CPv  $
      *
      *   Rev 2.3   Feb 25 2016 10:11:38   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:56:48   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:04:58   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 08 2015 11:16:04   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0282  FUNKTION SELECT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRAN0282                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT MIT VORSCHLAG <<<  TABELLE AGI TBAN0021       *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0282.
           05 PI-DRAN0282-AGI-KEY.
               10 PI-DRAN0282-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0282-AGI-HVANTNUM    PIC X(9).
      *
       01  PO-DRAN0282.
           05 PO-DRAN0282-RC                   PIC  X(003).
             88 PO-DRAN0282-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0282-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0282-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
      *   AGI-DATEN AUS PC-DATEN
           COPY DRAN0284.
      *
      ****** ENDE COPY-MEMBER ****************************************
 


