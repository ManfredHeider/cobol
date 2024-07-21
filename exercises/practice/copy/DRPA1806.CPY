      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1806.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1806.CPv  $
      *
      *   Rev 2.2   May 28 2015 09:55:12   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.1   Apr 23 2015 13:25:34   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.0   Mar 31 2015 12:57:54   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1806                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1806                    *
      *    BERECHNUNG PRÜFZIFFER ANTRAGSNUMMER PRODUKTGEBER AML (20)  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    31.03.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!31.03.15!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1806
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1806.
           05  PI-DRPA1806-KEY.
               10 PI-DRPA1806-PIN-13          PIC  9(13).
      *
       01  PO-DRPA1806.
           05  PO-DRPA1806-RC                 PIC  X(03).
               88 PO-DRPA1806-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1806-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRPA1806-PIN-PRF            PIC  X(02).
      *
      ******* ENDE COPY-MEMBER **************************************
 

