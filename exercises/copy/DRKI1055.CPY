      *---------------------------------------------------------------*
      *    Standard f™r alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1055.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1055.CPv  $
      *
      *   Rev 2.3   Mar 11 2004 10:03:24   t13ex49
      *Einführung AdresseOrtsteil
      *
      *   Rev 2.2   Oct 13 2003 11:14:40   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.1   Mar 30 2000 13:05:12   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:19:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1055                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÎR DIE FUNKTION DRKI1055                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.11.53
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
081003*         !08.10.03!    ! neue Attribute            ! ABM      *
      * -------------------------------------------------------------*
050304*         !05.03.02!    ! wieder neue Attribute     ! ABM      *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÕLT DIE EIN- UND AUSGABEPARAMETER
      *    FÎR DIE FUNKTION DRKI1055
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1055.
           05  PI-DRKI1055-KEY.
             10 PI-DRKI1055-ADRESSEID   PIC X(13).

           05  PI-DRKI1055-DATEN.
             10 PI-DRKI1055-ADRESSEBDSLDID       PIC X(03).
             10 PI-DRKI1055-ADRESSEISTVTADR      PIC X(01).
             10 PI-DRKI1055-ADRESSELANDID        PIC X(03).
             10 PI-DRKI1055-ADRESSEORT           PIC X(30).
             10 PI-DRKI1055-ADRESSEPLZ           PIC X(15).
             10 PI-DRKI1055-ADRESSESTRHAUSNUM    PIC X(45).
             10 PI-DRKI1055-LETZTEAENDERUNG      PIC X(26).
081003       10 PI-DRKI1055-ADRESSESUCHSTRASSE   PIC X(45).
081003       10 PI-DRKI1055-ADRESSEGEPRUEFT      PIC X(01).
050304       10 PI-DRKI1055-ADRESSEORTSTEIL      PIC X(030).
050304       10 PI-DRKI1055-ADRESSEPOSTPRUEF     PIC X(025).
050304       10 PI-DRKI1055-ADRESSEPOSTPRUEFID   PIC S9(004) COMP.


      *
       01  PO-DRKI1055.
           05  PO-DRKI1055-RC                 PIC  X(003).
               88 PO-DRKI1055-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1055-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1055-DATEN.
               10 PO-DRKI1055-PTRTABLENAME.
                  49 PO-DRKI1055-PTRTABLENAME-LEN   PIC S9(4) COMP.
                  49 PO-DRKI1055-PTRTABLENAME-TEXT  PIC X(40).
               10 PO-DRKI1055-PTRKEYDATA.
                  49 PO-DRKI1055-PTRKEYDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1055-PTRKEYDATA-TEXT    PIC X(200).
               10 PO-DRKI1055-PTRSQLDATA.
                  49 PO-DRKI1055-PTRSQLDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1055-PTRSQLDATA-TEXT    PIC X(3600).
      *
      ******* ENDE COPY-MEMBER **************************************
