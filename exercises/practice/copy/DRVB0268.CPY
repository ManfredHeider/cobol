      *****************************************************************
      *    --------------------------------------------------------   *
      *    COPY-MEMBER: DRVB0268                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0007                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    16.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASS00049!16.06.98! 01 ! NEUERSTELLUNG             ! WALB     *
      * AKI00001!12.07.98! 02 ! Erweiterung DRVB0252      !Zgur-Vujic*
      * AAS00078!02.10.98! 03 ! Erweiterung DRVB0323      ! WALB     *
      * ASL00024!02.11.98! 03 ! ERWEITERUNG DRVB0336      ! SCHMIDT,B*
      * ACV00007!05.11.01! 04 ! ERWEITERUNG DRVB0564      ! P.DUERR  *
      * ASL00149!02.12.02! 05 ! ERWEITERUNG DRVB0619      ! P.DUERR  *
120509* AVB01035!12.05.09! 06 ! NEUE FUNKTION DRVB0940    ! U.Boll   *
      *                                                              *
250422* JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *
250422* -------------------------------------------------------------*
250422* VLA-4642 !25.04.22! DRVB1339 HINZU UND PVCS-KOPF  !          *
250422*          !        ! ENTFERNT                      ! KLEEMEIER*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0007.
           05  PI-ZWVB0007-FUNKTION            PIC  X(008).
               88 PI-ZWVB0007-DRVB0269              VALUE 'DRVB0269'.
               88 PI-ZWVB0007-DRVB0252              VALUE 'DRVB0252'.
021098         88 PI-ZWVB0007-DRVB0323              VALUE 'DRVB0323'.
021198         88 PI-ZWVB0007-DRVB0336              VALUE 'DRVB0336'.
051101         88 PI-ZWVB0007-DRVB0564              VALUE 'DRVB0564'.
021202         88 PI-ZWVB0007-DRVB0619              VALUE 'DRVB0619'.
120509         88 PI-ZWVB0007-DRVB0941              VALUE 'DRVB0941'.
250422         88 PI-ZWVB0007-DRVB1339              VALUE 'DRVB1339'.
      *
      ******* ENDE COPY-MEMBER DRVB0268 *****************************
