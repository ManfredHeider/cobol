      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3346.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3346.CPv  $
      *
      *   Rev 2.1   Jun 27 2018 12:22:14   t13ac17
      *AKI01594: BKK-Linde Vertragslöschung - Sourcen
      *
      *   Rev 2.0   Jun 5 2018 11:46:23 T13AC17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3346                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3346                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         05.06.2018 / 11:46:23 / T13AC17
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
      *    FÜR DIE FUNKTION DRKI3346
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3346.
           05  PI-DRKI3346-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3346-G51PVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3346.
           05  PO-DRKI3346-RC                     PIC  X(003).
               88 PO-DRKI3346-RC-OK               VALUE 'OK '.
               88 PO-DRKI3346-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3346-DATEN.
             10   PO-DRKI3346-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

