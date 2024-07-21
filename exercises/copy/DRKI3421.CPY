      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3421.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3421.CPv  $
      *
      *   Rev 2.2   Sep 11 2018 11:28:32   t13ac23
      *AKI01629: Frischer Wind - Nacharbeiten löschen der gelöschte
      *n Kunden aus den KSDSen für Basisdaten - Sourcen
      *
      *   Rev 2.1   Sep 11 2018 10:38:58   t13ac23
      *AKI01629: Frischer Wind - Nacharbeiten löschen der gelöschte
      *n Kunden aus den KSDSen für Basisdaten - Sourcen
      *
      *   Rev 2.0   Sep 6 2018 10:28:16 T13AC23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3421                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3421                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.09.2018 / 10:28:16 / T13AC23
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
      *    FÜR DIE FUNKTION DRKI3421
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3421.
           05  PI-DRKI3421-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3421-GENVGESELLSCHID   PIC  X(0002).
             10 PI-DRKI3421-GENVVNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3421.
           05  PO-DRKI3421-RC                     PIC  X(003).
               88 PO-DRKI3421-RC-OK               VALUE 'OK '.
               88 PO-DRKI3421-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3421-DATEN.
             10   PO-DRKI3421-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
 

