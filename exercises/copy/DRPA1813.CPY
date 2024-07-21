      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1813.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1813.CPv  $
      *
      *   Rev 2.3   May 04 2015 09:17:40   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Strukt
      *urbereinigung DV MA-Wien
      *
      *   Rev 2.2   Apr 23 2015 16:28:18   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Strukt
      *urbereinigung DV MA-Wien
      *
      *   Rev 2.1   Apr 22 2015 11:12:12   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Strukt
      *urbereinigung DV MA-Wien
      *
      *   Rev 2.0   Apr 15 2015 13:06:30   t13ex24
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1813  STRUKTURBEREINIGUNG DV MA-WIEN     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Köprücü                                         *
      *    DATUM:    15.04.2015                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!15.04.15! 01 ! NEUERSTELLUNG (LUCY)      ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1813                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1813.
          05 PI-DRPA1813-PGEB-GES               PIC S9(004) BINARY.
          05 PI-DRPA1813-PROD-PROD-SCHL         PIC S9(004) BINARY.
          05 PI-DRPA1813-PROV-TERMIN            PIC  X(010).
 
       01 PO-DRPA1813.
          05 PO-DRPA1813-RC                     PIC  X(003).
             88 PO-DRPA1813-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1813-RC-VSTAT-GEFUELLT        VALUE 'VSG'.
          05 PO-DRPA1813-DATEN.
             10 PO-DRPA1813-STRK-KZ             PIC  X(001).
      *
      ******** ENDE COPY-MEMBER DRPA1813 *****************************
 

