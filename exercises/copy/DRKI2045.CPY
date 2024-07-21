      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2045.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2045.CPv  $
      *
      *   Rev 2.7   Oct 24 2007 16:06:32   t13ex14
      *AKL00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.2   Aug 09 2007 10:41:56   t13ex52
      *AKI00664: Allfinanz: Bestandsdaten, Kfz-Aktualisierung
      *
      *   Rev 2.1   Jul 09 2007 18:45:44   t13ex52
      *AKI00664: Allfinanz: Bestandsdaten, Kfz-Aktualisierung
      *
      *   Rev 2.0   Jul 06 2007 15:20:26   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2045                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2045                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.10.2007/15:58:50
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
      *    FÜR DIE FUNKTION DRKI2045
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2045.
           05  PI-DRKI2045-KEY.


             10 PI-DRKI2045-MOPAUFTRAG   PIC X(6).
             
      *
       01  PO-DRKI2045.
           05  PO-DRKI2045-RC                 PIC  X(003).
               88 PO-DRKI2045-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2045-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2045-DATEN.


             10 PO-DRKI2045-MOPMANDANTNR   PIC S9(9) COMP.
             10 PO-DRKI2045-MOPVERWFONDS   PIC S9(9) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
