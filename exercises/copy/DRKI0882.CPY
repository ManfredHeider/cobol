      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0882.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0882.CPv  $
      *
      *   Rev 2.1   Jul 02 1999 11:13:52   T13AE21
      *AKI00032
      *
      *   Rev 2.0   Apr 29 1999 13:57:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------* 
      *****************************************************************
      *    COPY-MEMBER: DRKI0882                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0882                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  02.07.99/08.33.06
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00032!02.07.99!2.0 ! Neuerstellung             ! PHILIPP  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI0882
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0882.
           05  PI-DRKI0882-KEY.


             10 PI-DRKI0882-VERVERTRAGSID   PIC X(13).
             

           05  PI-DRKI0882-DATEN.


             10 PI-DRKI0882-VEREIGENVERMKZ   PIC X(01).
             

      *
       01  PO-DRKI0882.
           05  PO-DRKI0882-RC                 PIC  X(003).
               88 PO-DRKI0882-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0882-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
