      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2673.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2673.CPv  $
      *
      *   Rev 2.2   Sep 06 2012 19:49:42   t13ex39
      *AKI01175: DeuBa-Anspracheschutz: Anpassungen in DeuBa-Aktual
      *isierung
      *
      *   Rev 2.1   Sep 06 2012 19:41:30   t13ex39
      *AKI01175: DeuBa-Anspracheschutz: Anpassungen in DeuBa-Aktual
      *isierung
      *
      *   Rev 2.0   Sep 5 2012 07:38:09 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2673                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2673                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         05.09.2012 / 07:38:09 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI2673
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2673.
           05  PI-DRKI2673-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2673-ASSVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2673-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2673-ASSABSCHLVBNR   PIC S9(9) COMP.
             10 PI-DRKI2673-ASSERSTELLTTST   PIC  X(026).
             10 PI-DRKI2673-ASSGESNR   PIC S9(4) COMP.
             10 PI-DRKI2673-ASSVNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI2673.
           05  PO-DRKI2673-RC                     PIC  X(003).
               88 PO-DRKI2673-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2673-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
