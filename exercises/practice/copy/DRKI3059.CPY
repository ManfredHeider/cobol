      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3059.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3059.CPv  $
      *
      *   Rev 2.2   Feb 24 2016 15:00:08   t13ex27
      *AKI01409: Abgleich-Kundenübertragungsart berücksichtigen
      *
      *   Rev 2.1   Feb 17 2016 15:55:40   t13ex27
      *AKI01409: Abgleich-Kundenübertragungsart berücksichtigen
      *
      *   Rev 2.0   Jan 28 2016 11:28:14   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3059                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3059                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01409!28.01.16!2.1 ! neu                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3059
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3059.
           05  PI-DRKI3059-KEY.
               10 PI-DRKI3059-HKUKUNDENID      PIC  X(13).
               10 PI-DRKI3059-HKUVBNRNEU       PIC S9(09)       BINARY.
               10 PI-DRKI3059-HKUSTEUERART     PIC  X(01).
               10 PI-DRKI3059-HKUTYP           PIC S9(04)       BINARY.

      *
       01  PO-DRKI3059.
           05  PO-DRKI3059-RC                 PIC  X(003).
               88 PO-DRKI3059-RC-OK           VALUE 'OK '.
               88 PO-DRKI3059-RC-NOT-FOUND    VALUE 'NFD'.
      *
           05  PO-DRKI3059-DATEN.
               10 PO-DRKI3059-HKUVBNRALT       PIC S9(09)       BINARY.
      *
      ******* ENDE COPY-MEMBER ***************************************


