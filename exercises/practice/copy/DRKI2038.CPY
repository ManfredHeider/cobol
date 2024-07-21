      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2038.CPv  $
      *
      *   Rev 2.1   Nov 09 2007 11:41:28   t13ex14
      *AKL00664: Kfz- u. REOB PK-Ermittlung u. Aktualisierung
      *
      *   Rev 2.0   Jul 03 2007 15:47:06   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2038                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2038                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.10.2007/10:42:46
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
      *    FÜR DIE FUNKTION DRKI2038
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2038.
           05  PI-DRKI2038-KEY.


             10 PI-DRKI2038-VERVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2038-DATEN.


             10 PI-DRKI2038-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2038-VERMANDANTNR   PIC S9(9) COMP.
             

      *
       01  PO-DRKI2038.
           05  PO-DRKI2038-RC                 PIC  X(003).
               88 PO-DRKI2038-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2038-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
