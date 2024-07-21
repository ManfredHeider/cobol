      *****************************************************************
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0934.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0934.CPv  $
      *
      *   Rev 2.2   Feb 15 2000 14:50:48   T13AE21
      * 
      *
      *   Rev 2.0   Aug 25 1999 09:28:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0934                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0934                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  25.08.99/15.05.14
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00043!26.08.99!    ! NEUERSTELLUNG             ! PHILIPP  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0934
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0934.
           05  PI-DRKI0934-KEY.


             10 PI-DRKI0934-STRUKTURSTUFEID   PIC S9(9) COMP.

      *
       01  PO-DRKI0934.
           05  PO-DRKI0934-RC                 PIC  X(003).
               88 PO-DRKI0934-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0934-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0934-DATEN.


             10 PO-DRKI0934-STRSTUFEKURZTEXT   PIC X(5).


      *
      ******* ENDE COPY-MEMBER **************************************
