      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1602.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1602.CPv  $
      *
      *   Rev 2.3   Feb 03 2016 14:59:24   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.2   Nov 24 2015 09:30:34   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.1   Oct 12 2004 08:14:52   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 15:41:16   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1602  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01932!24.11.15! 2.1! ERWEITERUNG               ! KÖPRÜCÜ  *
      * APA02208!01.02.16! 2.3! NEUE SATZLÄNGE WG. UST-JAH! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1602                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT-QSAM <<<                                      *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1602.
           05 PI-DRPA1602-UFKT         PIC  9(002).
241115*    05 PI-DRPA1602-DATEN        PIC  X(844).
010216*    05 PI-DRPA1602-DATEN        PIC  X(1344).
010216     05 PI-DRPA1602-DATEN        PIC  X(1346).
      *
        01 PO-DRPA1602.
           05 PO-DRPA1602-RC           PIC  X(003).
              88 PO-DRPA1602-RC-OK          VALUE 'OK '.
 

