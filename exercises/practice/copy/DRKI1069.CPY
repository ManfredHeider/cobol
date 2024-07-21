      *---------------------------------------------------------------*
      *    Standard fœr alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki1069.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI1069.CPv  $
      *
      *   Rev 2.2   Oct 13 2003 11:14:44   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.1   Mar 30 2000 13:05:42   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1069                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÎR DIE FUNKTION DRKI1069                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.25
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !05.08.03!    ! neu PERSABVERTRADRKZ      ! ABM      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÕLT DIE EIN- UND AUSGABEPARAMETER
      *    FÎR DIE FUNKTION DRKI1069
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1069.
           05  PI-DRKI1069-KEY.
             10 PI-DRKI1069-PERSABADRESSEID   PIC X(13).
             10 PI-DRKI1069-PERSABPERSONENID   PIC X(13).

           05  PI-DRKI1069-DATEN.
             10 PI-DRKI1069-LETZTEAENDERUNG     PIC X(26).
             10 PI-DRKI1069-PERSABADRESSARTID   PIC S9(4) COMP.
050803       10 PI-DRKI1069-PERSABVERTRADRKZ    PIC X(001).

      *
       01  PO-DRKI1069.
           05  PO-DRKI1069-RC                 PIC  X(003).
               88 PO-DRKI1069-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1069-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1069-DATEN.
               10 PO-DRKI1069-PTRTABLENAME.
                  49 PO-DRKI1069-PTRTABLENAME-LEN   PIC S9(4) COMP.
                  49 PO-DRKI1069-PTRTABLENAME-TEXT  PIC X(40).
               10 PO-DRKI1069-PTRKEYDATA.
                  49 PO-DRKI1069-PTRKEYDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1069-PTRKEYDATA-TEXT    PIC X(200).
               10 PO-DRKI1069-PTRSQLDATA.
                  49 PO-DRKI1069-PTRSQLDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1069-PTRSQLDATA-TEXT    PIC X(3600).
      *
      ******* ENDE COPY-MEMBER **************************************
