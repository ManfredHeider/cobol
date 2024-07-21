      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1815.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1815.CPv  $
      *
      *   Rev 2.1   Jun 08 2015 15:52:16   T13AE37
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Strukt
      *urbereinigung DV MA-Wien
      *
      *   Rev 2.0   Jun 01 2015 09:22:56   T13AE37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1815  STRUKTURBEREINIGUNG NICHTERKLÄRER  *
      *    -------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!01.06.15! 2.0! NEUERSTELLUNG (LUCY)      ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1815                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1815.
          05 PI-DRPA1815-PGEB-GES               PIC S9(004) BINARY.
          05 PI-DRPA1815-PROD-PROD-SCHL         PIC S9(004) BINARY.
          05 PI-DRPA1815-PROV-TERMIN            PIC  X(010).
 
       01 PO-DRPA1815.
          05 PO-DRPA1815-RC                     PIC  X(003).
             88 PO-DRPA1815-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1815-RC-VSTAT-GEFUELLT        VALUE 'VSG'.
          05 PO-DRPA1815-DATEN.
             10 PO-DRPA1815-STRK-KZ             PIC  X(001).
      *
      ******** ENDE COPY-MEMBER DRPA1813 *****************************

