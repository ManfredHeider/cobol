      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0219.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0219.CPv  $
      *
      *   Rev 2.3   Oct 01 2018 11:15:42   t13ae35
      *ASL00600: FAP-Leistungsprüfung 2018 für DVAG und Allfinanz
      *
      *   Rev 2.2   Mar 19 2008 15:08:02   t13ae35
      *ASL00285: Sonder- und Versorgungsleistungen für die Allfinan
      *z -> FAP (jhe)
      *
      *   Rev 2.1   30 Oct 2007 11:07:08   t13ae30
      *AVB00905: Allfinanz - Tabellenerweiterungen TWI
      *
      *   Rev 2.0   Feb 27 1999 15:58:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Apr 1998 08:51:30   T13AE35
      *AVB00399 - Neuerstellung, PI/PO fuer ZWVB0001
      *
      *   Rev 1.0   23 Jul 1997 12:17:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0219                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0219                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    23.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00399!23.03.98!    ! NEU                       ! HERING   *
      * AVB00905!20.06.07!    ! EINFÜHRUNG ALLFINANZ      ! WILLIG   *
      * ASL00285!19.03.08!    ! FVD                       ! J.Hering *
      * ASL00600!01.10.18!    ! ALAG                      ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0219: LESEN ST-NR
      *                               ERM. GESELLSCHAFTS-TYP
      *                               (1... DVAG, 2... AVBG)
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0219.
           05  PI-DRVB0219-DATEN.
               10 PI-DRVB0219-ST-NR            PIC S9(009) COMP.
      *
       01  PO-DRVB0219.
           05  PO-DRVB0219-RC                  PIC  X(003).
               88 PO-DRVB0219-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0219-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0219-DATEN.
200707         10 PO-DRVB0219-GESELL-BEZ       PIC  X(006).
200707             88 PO-DRVB0219-GESELL-DVAG    VALUE 'DVAG  '.
200707             88 PO-DRVB0219-GESELL-AVBG    VALUE 'AVBG  '.
200707             88 PO-DRVB0219-GESELL-ALLFIN  VALUE 'ALLFIN'.
190308             88 PO-DRVB0219-GESELL-FVD     VALUE 'FVD   '.
011018             88 PO-DRVB0219-GESELL-ALAG    VALUE 'ALAG  '.
      *
      ******* ENDE COPY-MEMBER **************************************

