      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1560.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1560.CPv  $
      *
      *   Rev 2.1   Jul 10 2003 13:47:28   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.0   May 19 2003 09:59:22   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1560                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1560                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  02.07.2003/12:32:02
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
      *    FÜR DIE FUNKTION DRKI1560
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1560.
           05  PI-DRKI1560-KEY.


             10 PI-DRKI1560-BILANZSCHL   PIC S9(4) COMP.

      *
       01  PO-DRKI1560.
           05  PO-DRKI1560-RC                 PIC  X(003).
               88 PO-DRKI1560-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1560-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1560-DATEN.


             10 PO-DRKI1560-BILANZSCHLTEXT   PIC X(30).


      *
      ******* ENDE COPY-MEMBER **************************************
