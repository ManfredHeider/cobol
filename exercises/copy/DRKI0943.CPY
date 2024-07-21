      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0943.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0943.CPv  $
      *
      *   Rev 2.2   Feb 08 2000 13:26:08   T13EX40
      *AKI00087
      *
      *   Rev 2.1   Nov 04 1999 14:36:46   T13EX40
      *Erweiterung für Bestandstrennung
      *
      *   Rev 2.0   Nov 04 1999 13:55:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0943                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0943                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.10.99/19.23.01
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI0943
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0943.
           05  PI-DRKI0943-KEY.
             10 PI-DRKI0943-CLIENT-NR   PIC S9(9) COMP.
             
      *
       01  PO-DRKI0943.
           05  PO-DRKI0943-RC                 PIC  X(003).
               88 PO-DRKI0943-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0943-RC-NOT-FOUND                 VALUE 'NFD'.
             

      *
      ******* ENDE COPY-MEMBER **************************************
