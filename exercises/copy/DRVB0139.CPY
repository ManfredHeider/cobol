      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0139.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0139.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Oct 1997 11:52:30   T13AE35
      *AFVB0364 - Neuerstellung, SST PMVB0107-ZWVB0014
      *
      *   Rev 1.0   Feb 19 1997 09:57:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0139                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0139                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Hering                                          *
      *    DATUM:    24.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0364!04.07.97! 01 ! NEU                       ! HERING   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0139
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0139.
           05  PI-DRVB0139-MAN-KEY.
               10 PI-DRVB0139-MAN-BEZUGS-NR   PIC S9(007) BINARY.
      *
       01  PO-DRVB0139.
           05  PO-DRVB0139-RC                 PIC  X(003).
               88 PO-DRVB0139-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0139-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0139-MAN-TITEL-AKAD     PIC  X(010).
           05  PO-DRVB0139-MAN-TITEL-ADEL     PIC  X(010).
           05  PO-DRVB0139-MAN-VORNAME        PIC  X(020).
           05  PO-DRVB0139-MAN-NAME           PIC  X(040).
           05  PO-DRVB0139-ANREDE             PIC  X(080).
      *
      ******* ENDE COPY-MEMBER **************************************
