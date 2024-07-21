      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2674.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2674.CPv  $
      *
      *   Rev 2.2   Sep 06 2012 19:49:50   t13ex39
      *AKI01175: DeuBa-Anspracheschutz: Anpassungen in DeuBa-Aktual
      *isierung
      *
      *   Rev 2.1   Sep 06 2012 19:41:38   t13ex39
      *AKI01175: DeuBa-Anspracheschutz: Anpassungen in DeuBa-Aktual
      *isierung
      *
      *   Rev 2.0   Sep 5 2012 07:38:12 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2674                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2674                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         05.09.2012 / 07:38:12 / T13EX39
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2674
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2674.
           05  PI-DRKI2674-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2674-ASSVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2674-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2674-ASSERLDAT   PIC  X(010).
             10 PI-DRKI2674-ASSERLKZ   PIC  X(0001).
END   *


      *
       01  PO-DRKI2674.
           05  PO-DRKI2674-RC                     PIC  X(003).
               88 PO-DRKI2674-RC-OK               VALUE 'OK '.
               88 PO-DRKI2674-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
