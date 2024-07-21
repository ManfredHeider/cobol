      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0810.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0810.CPv  $
      *
      *   Rev 2.1   Apr 08 1999 16:26:28   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:07:54   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   12 Jan 1999 15:48:00   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0810                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0810                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  26.01.99/08.58.42
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
      *    FÜR DIE FUNKTION DRKI0810
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0810.
           05  PI-DRKI0810-KEY.


             10 PI-DRKI0810-UMSGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0810-UMSVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0810.
           05  PO-DRKI0810-RC                 PIC  X(003).
               88 PO-DRKI0810-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0810-RC-NOT-FOUND                 VALUE 'NFD'.
      *

           05  PO-DRKI0810-DATEN.
             10 PO-DRKI0810-UMSLAUFENDENR   PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
