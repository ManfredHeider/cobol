      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.10.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0921.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0921.CPv  $
      *
      *   Rev 2.1   Oct 23 2008 10:17:56   t13ae36
      *AAS00788: Anpassung Postverteilerliste Allfinanz
      *
      *   Rev 2.0   Oct 22 2008 12:32:44   t13ae36
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB0921                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0921                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: R. Franke                                       *
      *    DATUM:    23.10.2008                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00788!23.10.08! 01 ! NEU (aus DRV0201)         ! R.Franke *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0921
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0921.
           05  PI-DRVB0921-DIR-KEY.
               10 PI-DRVB0921-DIR-NR         PIC S9(004) BINARY.
      *
       01  PO-DRVB0921.
           05  PO-DRVB0921-RC                 PIC  X(003).
               88 PO-DRVB0921-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0921-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
